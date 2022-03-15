use deku::prelude::*;

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct Boolean(pub bool);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct Byte(pub i8);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct UnsignedByte(pub u8);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct Short(pub i16);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct UnsignedShort(pub u16);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct Int(pub i32);

#[derive(Debug, PartialEq, DekuRead, DekuWrite)]
#[deku(endian = "big")]
pub struct Long(pub i64);

pub struct Float(pub f32);

pub struct Double(pub f64);

pub struct String<const N: usize> {
    size: VarInt,
    string: std::string::String,
}

impl<const N: usize> String<N> {
    pub fn new(s: &str) -> String<N> {
        todo!()
    }
}

pub struct Chat(pub String<26_2144>);

pub struct Identifier(pub String<32_767>);

pub struct VarInt(pub i32);

pub struct VarLong(pub i64);

pub struct EntityMetadata(TODO);

pub struct Slot(TODO);

pub struct NbtTag(TODO);

pub struct Position(TODO);

pub struct Angle(pub u8);

pub struct Uuid(TODO);

pub struct Optional<T>(pub Option<T>);

pub struct Array<T>(pub Vec<T>);

pub struct Enum<T>(pub T);

pub struct ByteArray(pub Vec<u8>);

pub enum TODO {}
