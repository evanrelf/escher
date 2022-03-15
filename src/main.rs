#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_variables)]

mod packets;
mod types;

use crate::{packets::*, types::*};

fn main() {
    println!("Hello world");
    // server();
}

// fn client() {
//     send(Handshake::new(VarInt(0x00), HandshakeData { }));
//     println!("Sent handshake");
//
//     send(StatusRequest::new(0x00, ()));
//     println!("Sent status request");
//
//     let packet = receive();
//     println!("{packet}");
// }
//
// fn server() {
// }
//
// fn receive<D>() -> PacketWith<D> {
//     todo!()
// }
//
// fn send<D>(packet: PacketWith<D>) {
//     todo!();
// }
