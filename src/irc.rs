use std::io::BufReader;
use std::io::Reader;
use std::io::net::tcp::TcpStream;

pub struct IRC {
    server: String,
    port:   u16,
    nick:   String,
    conn:   TcpStream,
    buffer: Vec<u8>
}

impl IRC {
    pub fn connect(server: String, port: u16, nick: String) -> IRC {
        let conn = TcpStream::connect(server[], port).ok().unwrap();

        IRC {
            server: server,
            port:   port,
            nick:   nick,
            conn:   conn,
            buffer: Vec::new()
        }
    }

    pub fn raw(&mut self, data: &str) {
        println!("-> {}", data);
        self.conn.write_line(data);
        self.conn.flush();
    }

    pub fn read_line(&mut self) -> Option<String> {
        self.conn.set_timeout(Some(500));
        self.conn.push(128, &mut self.buffer);

        if !self.buffer.is_empty() {
            /* Use a BufReader to pop a String off. */
            let (len, data) = {
                let mut buffer = BufReader::new(self.buffer.as_slice());

                /* Try and read a line off the buffer, check if it doesn't
                 * end with a newline in order to skip. */
                let data = match buffer.read_until('\n' as u8) {
                    Ok(v) => {
                        if v[v.len() - 1] != '\n' as u8 {
                            None
                        } else {
                            Some(v)
                        }
                    }

                    Err(_) => None
                };

                (buffer.tell().unwrap(), data)
            };

            match data {
                Some(v) => {
                    let data         = String::from_utf8(v).unwrap();
                    let current_size = self.buffer.len();

                    /* Really really inefficient, figure out a better way to
                    * do this in future. */
                    self.buffer.reverse();
                    self.buffer.truncate(current_size - (len as uint));
                    self.buffer.reverse();

                    Some(data)
                }

                _ => {
                    None
                }
            }
        } else {
            None
        }
    }
}
