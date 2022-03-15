use binrw::{
    io::{prelude::*, SeekFrom},
    BinRead, Error,
};

pub struct Boolean(pub bool);

impl BinRead for Boolean {
    type Args = ();

    fn read_options<R: Read + Seek>(
        reader: &mut R,
        options: &binrw::ReadOptions,
        _: Self::Args,
    ) -> binrw::BinResult<Self> {
        let mut val = [0; 1];
        let pos = reader.stream_position()?;

        reader.read_exact(&mut val).or_else(|e| {
            reader.seek(SeekFrom::Start(pos))?;
            Err(e)
        })?;

        match val {
            [0x00] => Ok(Boolean(false)),
            [0x01] => Ok(Boolean(true)),
            _ => Err(Error::NoVariantMatch { pos: 0 }),
        }
    }
}

impl BinWrite for Boolean {

}

pub struct Byte(pub i8);

pub struct UnsignedByte(pub u8);

pub struct Short(pub i16);

pub struct UnsignedShort(pub u16);

pub struct Int(pub i32);

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
