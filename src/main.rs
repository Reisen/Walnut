#![feature(phase)]
#![feature(tuple_indexing)]
#![feature(if_let)]
#![feature(while_let)]
#![feature(slicing_syntax)]
#![feature(macro_rules)]


#[phase(plugin)]
extern crate regex_macros;
extern crate regex;
extern crate redis;
extern crate toml;


mod irc;
mod hooks;
mod config;


fn main() {
    use redis::{Client, Cmd};


    // Parse the configuration file.
    let config =
        if let Ok(table) = config::parse() {
            table
        } else {
            fail!("Error parsing configuration file.");
            return;
        };


    // Channel used for sending incoming Redis messages to IRC.
    type ChanType = (Sender<(String, String)>, Receiver<(String, String)>);
    let (tx, rx): ChanType = channel();


    // Thread that forwards Redis messages to the IRC thread.
    spawn(proc() {
        let client = Client::open("redis://127.0.0.1").unwrap();
        let client = client.get_connection().unwrap();

        // PubSub object for receiving messages.
        let pubsub   = Client::open("redis://127.0.0.1/").unwrap();
        let mut conn = pubsub.get_pubsub().unwrap();

        // PubSub channels.
        conn.psubscribe("SND.*").unwrap();
        conn.psubscribe("QRY.*").unwrap();

        // Loop forever waiting for Redis messages.
        while let Ok(result) = conn.get_message() {
            // Find out which channel the messages came from.
            let channel = result.get_channel_name();

            // Respond to bot core queries.
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

            // Forward 'sent' messages to the IRC thread.
            if channel.starts_with("SND.") {
                tx.send((
                    String::from_str(result.get_channel_name()),
                    result.get_payload().unwrap()
                ));
            }
        }
    });


    // IRC thread the receives IRC messages and pipes them out.
    spawn(proc() {
        use std::io::File;
        use std::str::from_utf8;

        let client = Client::open("redis://127.0.0.1").unwrap();
        let client = client.get_connection().unwrap();

        // We need a mutable collection of open IRC servers as well, which we
        // will constantly wait on messages on.
        let mut conns = Vec::new();

        // Actually need to connect for that matter.
        if let toml::Array(ref servers) = config["servers".to_string()] {
            for server in servers.iter() {
                if let Ok(irc) = config::to_irc(server.as_table().unwrap()) {
                    conns.push(irc)
                }
            }
        }

        loop {
            let mut incoming = Vec::new();

            // Receive all the Redis messages that are piped to us, and put them
            // on the incoming queue to be processed.
            while let Ok(v) = rx.try_recv() {
                incoming.push(v);
            }

            // Cycle through the connections, we do the message forwarding for
            // each open connection so each connection must have some form of
            // timeout or other connections will end up hung.
            for conn in conns.iter_mut() {
                // Unwrap the connection, as it contains its unique identifier
                // used for message routing.
                let (ref ident, ref mut conn) = *conn;

                // Try and read a line from the connection.
                let data = conn.read_line();

                // If the read was successful, we parse the line and extract the
                // IRC parts from it.
                if let Some(Some((prefix, command, args))) = data.as_ref().map(irc::parse) {
                    // Get a reference to the data (this operation is safe, we
                    // would not be in the if if data had been None).
                    let data = data.as_ref().unwrap();

                    // Logs for debugging purposes.
                    print!("<- {}", data);

                    // Handle PING specially, so we don't die just because the
                    // plugin that was meant to be handling this failed.
                    if command[] == "PING" {
                        conn.raw(format!("PONG {}", args[])[]);
                    }

                    Cmd::new()
                        .arg("PUBLISH")
                        .arg(format!("RCV.{}:{}", command[], ident))
                        .arg(data[])
                        .execute(&client);
                }

                // Any Redis messages pushed accross the task boundary need to
                // be forwarded to a target server.
                incoming = incoming
                    .into_iter()
                    .filter_map(|v| {
                        if v.0[].contains(ident[]) {
                            conn.raw(v.1[]);
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
