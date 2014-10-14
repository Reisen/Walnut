#![feature(phase)]
#![feature(tuple_indexing)]
#![feature(if_let)]
#![feature(slicing_syntax)]


#[phase(plugin)]
extern crate regex_macros;
extern crate hiredis;
extern crate regex;
extern crate redis;
extern crate toml;


mod irc;
mod hooks;


#[allow(non_snake_case)]
fn main() {
    use redis::{
        Client,
        Cmd,
        Value
    };

    /* Two channels are defined so that messages can be sent backward
     * and forward between the two threads.
     */
    type ChanType = (Sender<(String, String)>, Receiver<(String, String)>);

    let (sIRC, rIRC): ChanType = channel();
    let (sRED, rRED): ChanType = channel();


    /* We spawn the PubSub thread. This listens for incoming messages
     * to be piped to IRC and also responds to QRY messages which are
     * used to send state information to clients.
     */
    spawn(proc() {
        let client = Client::open("redis://127.0.0.1").unwrap();
        let client = client.get_connection().unwrap();

        /* PubSub object for receiving messages. */
        let pubsub   = Client::open("redis://127.0.0.1").unwrap();
        let mut conn = pubsub.get_pubsub().unwrap();

        /* PubSub channels. */
        conn.psubscribe("SND.*").unwrap();
        conn.psubscribe("QRY.*").unwrap();

        /* Loop forever waiting for Redis messages. */
        loop {
            let result  = conn.get_message().unwrap();
            let channel = result.get_channel_name();

            /* Respond to bot core queries. */
            if channel.starts_with("QRY.") {
                let commands = [
                    ("WHOAMI", hooks::whoami)
                ];

                for &(command, hook) in commands.iter() {
                    if channel.contains(command) {
                        let response = hook(result.get_payload().unwrap());
                        Cmd::new()
                            .arg("PUBLISH")
                            .arg(format!("RSP.{}", command))
                            .arg(response[])
                            .execute(&client);
                    }
                }
            }

            /* Forward 'sent' messages to the IRC thread. */
            if channel.starts_with("SND.") {
                sIRC.send((
                    String::from_str(result.get_channel_name()),
                    result.get_payload().unwrap()
                ));
            }
        }
    });

    spawn(proc() {
        use std::io::File;
        use std::str::from_utf8;

        let client = Client::open("redis://127.0.0.1").unwrap();
        let client = client.get_connection().unwrap();

        /* We need a mutable collection of open IRC servers as well, which we will
         * constantly wait on messages on. */
        let mut conns = Vec::new();

        /* Actually need to connect for that matter. */
        if let Ok(data) = File::open(&Path::new("config.ini")).read_to_end() {
            let data = toml::Parser::new(from_utf8(data[]).unwrap()).parse();

            if let None = data {
                println!("Data parsed from config.ini not valid toml.");
                return;
            }

            let data = data.unwrap();

            if let toml::Array(ref servers) = data["servers".to_string()] {
                for server in servers.iter() {
                    let server = server.as_table().unwrap();

                    /* Connect to IRC using the extracted server details. */
                    let address = server["address".to_string()].as_str().unwrap();
                    let port    = server["port".to_string()].as_integer().unwrap();
                    let nick    = server["nick".to_string()].as_str().unwrap();
                    let pass    = server.find(&"pass".to_string());
                    let mut con = irc::IRC::connect(address.to_string(), port as u16, nick.to_string());

                    /* Password only if given, needs to be done before anything else. */
                    if let Some(pass) = pass {
                        con.raw(format!("PASS {}", pass.as_str().unwrap())[]);
                    }

                    /* User information. */
                    con.raw(format!("NICK {}", nick)[]);
                    con.raw(format!("USER {0} {0} {0} :{0}", nick)[]);

                    /* Join all the specified channels. */
                    for channel in server["channels".to_string()].as_slice().unwrap().iter() {
                        con.raw(format!("JOIN {}", channel.as_str().unwrap())[]);
                    }

                    conns.push((address.to_string(), con));
                }
            }
        }

        loop {
            let mut incoming = Vec::new();

            /* I want `while let` so bad. This will look so much nicer if
             * I can simply do: while let Ok(v) = rIRC.try_recv() {
             *
             * The recommended solution right now is to use an iterator but
             * lets be honest if I try and iterate here with the current
             * protocol I'll be blocked forever.
             */
            loop {
                match rIRC.try_recv() {
                    Ok(v) => {
                        incoming.push(v);
                    }

                    Err(_) => {
                        break;
                    }
                }
            }

            for conn in conns.iter_mut() {
                let (ref ident, ref mut c) = *conn;

                match c.read_line() {
                    Some(data) => {
                        let parser = regex!(r"^(:\S+)?\s*(\S+)\s+(.*)\r?$");
                        let chars: &[_] = &[' ', '\n', '\r'];

                        /* Parse PREFIX/COMMAND/ARGS from incoming messages to
                         * forward to Redis. */
                        match parser.captures(data[].trim_chars(chars)) {
                            /* v.at(1): PREFIX
                             * v.at(2): COMMAND
                             * v.at(3): ARGS
                             */
                            Some(v) => {
                                /* Print the line out for logs. */
                                print!("<- {}", data);

                                /* Handle PING specially, so we don't die just
                                 * because the plugin that was meant to be handling
                                 * this failed.
                                 */
                                if v.at(2)[] == "PING" {
                                    c.raw(format!("PONG {}", v.at(3))[]);
                                }
                                else {
                                    Cmd::new()
                                        .arg("PUBLISH")
                                        .arg(format!("RCV.{}:{}", v.at(2), ident))
                                        .arg(data[])
                                        .execute(&client);
                                }
                            }

                            None => {
                            }
                        }
                    }

                    _ => {
                    }
                }

                /* Any Redis messages pushed accross the task boundary need to
                 * be forwarded to a target server. This is pretty damn straight
                 * forward and doesn't need much explanation. */
                incoming = incoming
                    .into_iter()
                    .filter_map(|v| {
                        if v.0[].contains(ident[]) {
                            c.raw(v.1[]);
                            None
                        } else {
                            Some(v)
                        }
                    })
                    .collect();
            }
        }
    });
}
