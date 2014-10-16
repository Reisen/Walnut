use toml::{Parser, Array, Value};
use irc::IRC;
use std::collections::TreeMap;
use std::str::from_utf8;
use std::io::{
    IoResult,
    IoError,
    InvalidInput,
    File
};



pub fn parse() -> IoResult<TreeMap<String, Value>> {
    /* Open the file for reading, make sure the encoding is right. */
    let data = try!(File::open(&Path::new("config.ini")).read_to_end());
    let data = from_utf8(data[]);

    if let None = data {
        return Err(IoError {
            kind:   InvalidInput,
            desc:   "Invalid characters in the config file.",
            detail: None
        });
    }


    /* Parse the data as TOML. Make sure it is valid. */
    let data = Parser::new(data.unwrap()).parse();

    if let None = data {
        return Err(IoError {
            kind:   InvalidInput,
            desc:   "File is not valid TOML.",
            detail: None
        });
    }


    Ok(data.unwrap())
}



pub fn to_irc(server: &TreeMap<String, Value>) -> IoResult<(String, IRC)> {
    macro_rules! extract_value(
        ($target:expr, $method:ident) => {
            match server[$target.to_string()].$method() {
                Some(v) => v,
                None    => {
                    return Err(IoError {
                        kind:   InvalidInput,
                        desc:   "Field missing or of incorrect type.",
                        detail: None
                    });
                }
            }
        }
    )

    /* Extract server details and connect. */
    let address = extract_value!("address", as_str);
    let port    = extract_value!("port", as_integer);
    let nick    = extract_value!("nick", as_str);
    let pass    = server.find(&"pass".to_string());
    let mut irc = IRC::connect(
        address.to_string(),
        port as u16,
        nick.to_string()
    );

    /* Authenticate if necessary. */
    if let Some(pass) = pass {
        irc.raw(format!("PASS {}", pass.as_str().unwrap())[]);
    }

    /* Join anything that needs joining, send user info etc. */
    irc.raw(format!("NICK {}", nick)[]);
    irc.raw(format!("USER {0} {0} {0} :{0}", nick)[]);

    for channel in extract_value!("channels", as_slice).iter() {
        irc.raw(format!("JOIN {}", channel.as_str().unwrap())[]);
    }

    Ok((address.to_string(), irc))
}
