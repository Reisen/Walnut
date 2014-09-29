use std::io::BufferedReader;
use std::io::net::tcp::TcpStream;

pub struct IRC {
    server: String,
    port:   u16,
    nick:   String,
    conn:   Option<BufferedReader<TcpStream>>
}

impl IRC {
    pub fn connect(server: &str, port: u16, nick: &str) -> IRC {
        let conn = TcpStream::connect(server, port).ok().unwrap();
        let conn = BufferedReader::new(conn);

        IRC {
            server: String::from_str(server),
            port:   port,
            nick:   String::from_str(nick),
            conn:   Some(conn)
        }
    }

    pub fn read_line(&mut self) -> String {
        println!("Waiting for Line...");
        match self.conn {
            Some(ref mut v) => {
                match v.read_line() {
                    Ok(v)  => v,
                    Err(e) => {
                        println!("Error: {}", e);
                        String::from_str("")
                    }
                }
            }

            None => String::from_str("")
        }
    }
}
