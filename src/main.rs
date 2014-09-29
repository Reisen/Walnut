extern crate hiredis;
extern crate redis;

mod irc;

fn main() {
    use redis::{
        Client,
        Cmd,
        Value
    };

    // let client = Client::open("redis://127.0.0.1").unwrap();
    // let conn   = client.get_connection().unwrap();

    // let resp: Value = Cmd::new()
    //     .arg("PSUBSCRIBE")
    //     .arg("IRC.*")
    //     .query(&conn)
    //     .unwrap();

    // loop {
    //     let result: (String, String, String, String) =
    //         conn.read_response().unwrap();

    //     println!("Message: {}", result);
    // }

    // use std::io::timer;
    // use std::time::duration::Duration;

    // /* Setup the Redis side of things. */
    // let redis  = hiredis::Redis::new("127.0.0.1", 6379, 5);
    // let result = redis.command("PSUBSCRIBE IRC.*");

    /* And setup the IRC side of things. */
    let mut conns = Vec::new();
    conns.push(irc::IRC::connect("irc.rizon.net", 6667, "rustnut"));

    loop {
        for conn in conns.mut_iter() {
            let incoming_data = conn.read_line();
            println!("Data: {}", incoming_data);
        }
    }

    // /* We create an empty reply object to use for parsing replies. It
    //  * can be re-used as the receive function will destroy the old reply
    //  * data before overwriting it.
    //  *
    //  * TODO: Check that the data can't leak in the hiredis project.
    //  */
    // let mut reply = hiredis::Reply::empty();

    // loop {
    //     /* TODO: Switch to while let once implemented. */
    //     let rec = redis.receive(&mut reply);

    //     println!("rec: {}", rec);

    //     if rec {
    //         println!("Received");

    //         for v in reply.array().iter() {
    //             match v.typename() {
    //                 hiredis::String => {
    //                     use std::str;

    //                     let data = v.string().unwrap();
    //                     let data = str::from_utf8(data);

    //                     match data {
    //                         Some(v) => println!("String: {}", v),
    //                         None    => {}
    //                     };
    //                 }

    //                 _ => {
    //                 }
    //             }
    //         }
    //     }

    //     println!("Tick");
    // }
}
