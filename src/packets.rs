use crate::types::*;

pub struct Packet<D> {
    pub length: VarInt,
    pub id: VarInt,
    pub data: D,
}

impl<D> Packet<D> {
    pub fn new(id: VarInt, data: D) -> Packet<D> {
        let length = VarInt(todo!());
        Packet { length, id, data }
    }
}

pub struct HandshakeData {
    pub protocol_version: VarInt,
    pub server_address: String<255>,
    pub server_port: UnsignedShort,
    pub next_state: Enum<VarInt>,
}

pub struct Handshake(pub Packet<HandshakeData>);

pub struct StatusRequest(pub Packet<()>);

pub struct StatusResponseData {
    pub json: String<0>,
}

pub struct StatusResponse(pub Packet<StatusResponseData>);

impl Default for StatusResponse {
    fn default() -> StatusResponse {
        let json = String::new(
            &serde_json::json!({
                "version": {
                    "name": "1.17.1",
                    "protocol": 756,
                },
                "players": {
                    "max": 10,
                    "online": 0,
                    "sample": []
                },
                "description": {
                    "text": "Hello world"
                }
            })
            .to_string(),
        );
        StatusResponse(Packet::new(VarInt(0x00), StatusResponseData { json }))
    }
}

pub struct Ping(pub Packet<Long>);

pub struct Pong(pub Packet<Long>);

impl Pong {
    pub fn new(n: Long) -> Pong {
        Pong(Packet::new(VarInt(0x01), n))
    }
}
