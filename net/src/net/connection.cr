module Net
    abstract class Connection
        abstract def connect
        abstract def disconnect
        abstract def send
        abstract def recv
    end
end
