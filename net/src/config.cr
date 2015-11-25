require "json"

module Config
    class IRC
        JSON.mapping({
            addr: String,
            port: Int32,
            nick: String,
            chan: Array(String)
        })
    end

    class XMPP
        JSON.mapping({
            network: String
        })
    end

    alias Protocols = IRC | XMPP

    class Configuration
        JSON.mapping({
            irc: Array(IRC),
            xmpp: Array(XMPP)
        })
    end
end
