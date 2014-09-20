extern crate hiredis;

fn main() {
    let redis  = hiredis::Redis::new("127.0.0.1", 6379);

    let result = redis.command("PSUBSCRIBE IRC.*");

    loop {
        println!("\nFetching");
        let result = match redis.receive() {
            Some(v) => v,
            None    => { break; }
        };

        println!(
            "Result Type: {} -> {}",
            result.typename() as uint,
            result.string()
        );
    }
}
