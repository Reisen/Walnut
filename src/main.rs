extern crate hiredis;

fn main() {
    let redis  = hiredis::Redis::new("127.0.0.1", 6379);
    let result = redis.command("SUBSCRIBE IRC");

    loop {
        let result = match redis.receive() {
            Some(v) => v,
            None    => { break; }
        };

        println!("Num: {}", result.typename() as uint);
    }
}
