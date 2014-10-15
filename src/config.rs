use toml::{Parser, Array, Value};
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
