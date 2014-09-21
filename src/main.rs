extern crate hiredis;

fn main() {
    let redis  = hiredis::Redis::new("127.0.0.1", 6379);
    let result = redis.command("SUBSCRIBE IRC");

    let mut reply = hiredis::Reply::empty();

    loop {
        redis.receive(&mut reply);
        println!("Num: {}", reply.typename() as uint);
    }
}
