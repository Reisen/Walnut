use std::io::BufReader;
use std::io::Reader;
use std::io::net::tcp::TcpStream;

/// Structure representing an open IRC connection.
pub struct IRC {
    server: String,
    port:   u16,
    nick:   String,
    conn:   TcpStream,
    buffer: Vec<u8>
}



/// Parse an line of text containing an IRC message.
pub fn parse<'a>(line: &'a String) -> Option<(&'a str, &'a str, &'a str)> {
    let parser      = regex!(r"^(:\S+)?\s*(\S+)\s+(.*)\r?$");
    let chars: &[_] = &[' ', '\n', '\r'];

    if let Some(data) = parser.captures(line[].trim_chars(chars)) {
        Some((
            data.at(1),
            data.at(2),
            data.at(3)
        ))
    } else {
        None
    }
}



impl IRC {
    /// Connect to a server based on an addrses and port.
    pub fn connect(server: String, port: u16, nick: String) -> IRC {
        let conn = TcpStream::connect((server[], port)).ok().unwrap();

        IRC {
            server: server,
            port:   port,
            nick:   nick,
            conn:   conn,
            buffer: Vec::new()
        }
    }

    /// Send a raw IRC message, the call will will automatically deal with \r\n.
    pub fn raw(&mut self, data: &str) {
        println!("-> {}", data);
        self.conn.write_line(data);
        self.conn.flush();
    }

    /// Read a line from the server, will time out after half a second.
    pub fn read_line(&mut self) -> Option<String> {
        self.conn.set_timeout(Some(500));
        self.conn.push(128, &mut self.buffer);

        if !self.buffer.is_empty() {
            // Use a BufReader to pop a String off.
            let (len, data) = {
                let mut buffer = BufReader::new(self.buffer.as_slice());

                // Try and read a line off the buffer, check if it doesn't end
                // with a newline in order to skip.
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

                    // Really really inefficient, figure out a better way to do
                    // this in future.
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
