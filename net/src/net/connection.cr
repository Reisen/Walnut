module Net
    abstract class Connection
        abstract def connect
        abstract def disconnect
        abstract def send
        abstract def recv
        abstract def conn
    end
end
