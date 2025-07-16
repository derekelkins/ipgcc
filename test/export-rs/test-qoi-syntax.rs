#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unused_variables)]

#[derive(Debug)]
enum ChunkType {
    RGB(u8, u8, u8),
    RGBA(u8, u8, u8, u8),
    Index(u8),
    Diff(u8, u8, u8),
    Luma(u8, u8, u8),
    Run(u8),
}

use ChunkType::*;

fn wrapping_sub(x: u8, y: u8) -> u8 { x.wrapping_sub(y) }
#[derive(Debug)]
struct Chunk {
  chunk: ChunkType,
  tag: Vec<u8>,
}

#[derive(Debug)]
struct QOI {
  channels: u8,
  chunks: Vec<ChunkType>,
  colorspace: u8,
  height: i64,
  width: i64,
}

#[derive(Debug)]
struct QOIHeader {
  channels: u8,
  colorspace: u8,
  height: i64,
  width: i64,
}

#[derive(Debug)]
struct QOIChunks {
  values: Vec<ChunkType>,
}

#[derive(Debug)]
struct QOIChunk {
  chunk: ChunkType,
  tagByte: u8,
}

#[derive(Debug)]
struct EndMarker {
}

#[derive(Debug)]
struct U8 {
  value: u8,
}

#[derive(Debug)]
struct BE_U32 {
  bs: Vec<u8>,
  value: i64,
}

fn QOI(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, QOI)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // QOIHeader@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_QOIHeader_0_m = QOIHeader(input, begin + left, begin + right);
    let (mut nt_QOIHeader_0_ipg_start, mut nt_QOIHeader_0_ipg_end, nt_QOIHeader_0) = match nt_QOIHeader_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_QOIHeader_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_QOIHeader_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_QOIHeader_0_ipg_end);
    }
    nt_QOIHeader_0_ipg_end += left;
    nt_QOIHeader_0_ipg_start += left;
    left = nt_QOIHeader_0_ipg_start;
    right = nt_QOIHeader_0_ipg_end;

    // { width = QOIHeader@0.width }
    let mut self_width = nt_QOIHeader_0.width;

    // { height = QOIHeader@0.height }
    let mut self_height = nt_QOIHeader_0.height;

    // { channels = QOIHeader@0.channels }
    let mut self_channels = nt_QOIHeader_0.channels;

    // { colorspace = QOIHeader@0.colorspace }
    let mut self_colorspace = nt_QOIHeader_0.colorspace;

    // QOIChunks@0[QOIHeader@0.END, EOI]
    left = nt_QOIHeader_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_QOIChunks_0_m = QOIChunks(input, begin + left, begin + right);
    let (mut nt_QOIChunks_0_ipg_start, mut nt_QOIChunks_0_ipg_end, nt_QOIChunks_0) = match nt_QOIChunks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_QOIChunks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_QOIChunks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_QOIChunks_0_ipg_end);
    }
    nt_QOIChunks_0_ipg_end += left;
    nt_QOIChunks_0_ipg_start += left;
    left = nt_QOIChunks_0_ipg_start;
    right = nt_QOIChunks_0_ipg_end;

    // { chunks = QOIChunks@0.values }
    let mut self_chunks = nt_QOIChunks_0.values;

    return Some((self_ipg_start, self_ipg_end, QOI {
      channels: self_channels,
      chunks: self_chunks,
      colorspace: self_colorspace,
      height: self_height,
      width: self_width,
    }));
  }

  return None;
}

fn QOIHeader(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, QOIHeader)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "qoif"[0, 4]
    left = 0 as usize;
    right = 4 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[113, 111, 105, 102]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 4;
    self_ipg_end = self_ipg_end.max(right);

    // BE_U32@0[4, EOI]
    left = 4 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_0_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_0_ipg_start, mut nt_BE_U32_0_ipg_end, nt_BE_U32_0) = match nt_BE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_0_ipg_end);
    }
    nt_BE_U32_0_ipg_end += left;
    nt_BE_U32_0_ipg_start += left;
    left = nt_BE_U32_0_ipg_start;
    right = nt_BE_U32_0_ipg_end;

    // { width = BE_U32@0.value }
    let mut self_width = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, EOI]
    left = nt_BE_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_1_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_1_ipg_start, mut nt_BE_U32_1_ipg_end, nt_BE_U32_1) = match nt_BE_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_1_ipg_end);
    }
    nt_BE_U32_1_ipg_end += left;
    nt_BE_U32_1_ipg_start += left;
    left = nt_BE_U32_1_ipg_start;
    right = nt_BE_U32_1_ipg_end;

    // { height = BE_U32@1.value }
    let mut self_height = nt_BE_U32_1.value;

    // { channels = .[BE_U32@1.END] }
    left = nt_BE_U32_1_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_channels = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { colorspace = .[BE_U32@1.END + 1] }
    left = (nt_BE_U32_1_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_colorspace = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, QOIHeader {
      channels: self_channels,
      colorspace: self_colorspace,
      height: self_height,
      width: self_width,
    }));
  }

  return None;
}

fn QOIChunks(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, QOIChunks)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat QOIChunk@0[QOIChunk@0.END, EOI].chunk starting on [0, EOI] until EndMarker@0
    left = 0 as usize;
    right = EOI as usize;
    let mut self_values = Vec::new();
    let mut nt_EndMarker_0_ipg_start = right;
    let mut nt_EndMarker_0_ipg_end = left;
    loop {
      if right < left || right > EOI { break '_ipg_alt; }
      let nt_EndMarker_0_m = EndMarker(input, begin + left, begin + right);
      match nt_EndMarker_0_m {
        None => {}
        Some((nt_EndMarker_0_ipg_start_, nt_EndMarker_0_ipg_end_, nt_EndMarker_0)) => {
          nt_EndMarker_0_ipg_start = nt_EndMarker_0_ipg_start_;
          nt_EndMarker_0_ipg_end = nt_EndMarker_0_ipg_end_;
          if nt_EndMarker_0_ipg_end != 0 {
            self_ipg_start = self_ipg_start.min(left + nt_EndMarker_0_ipg_start);
            self_ipg_end = self_ipg_end.max(left + nt_EndMarker_0_ipg_end);
          }
          nt_EndMarker_0_ipg_end += left;
          nt_EndMarker_0_ipg_start += left;
          right = nt_EndMarker_0_ipg_end;
          break;
        }
      };
      let nt_QOIChunk_0_m = QOIChunk(input, begin + left, begin + right);
      let (mut nt_QOIChunk_0_ipg_start, mut nt_QOIChunk_0_ipg_end, nt_QOIChunk_0) = match nt_QOIChunk_0_m {
        None => { break '_ipg_alt; }
        Some(p) => p,
      };
      if nt_QOIChunk_0_ipg_end == 0 { panic!("repeat of non-consuming rule: QOIChunk"); }
      self_ipg_start = self_ipg_start.min(left + nt_QOIChunk_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_QOIChunk_0_ipg_end);
      nt_QOIChunk_0_ipg_end += left;
      nt_QOIChunk_0_ipg_start += left;
      self_values.push(nt_QOIChunk_0.chunk);
      left = nt_QOIChunk_0_ipg_end as usize;
      right = EOI as usize;
    }

    return Some((self_ipg_start, self_ipg_end, QOIChunks {
      values: self_values,
    }));
  }

  return None;
}

fn QOIChunk(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, QOIChunk)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { tagByte = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_tagByte = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // Chunk@0(tagByte)[1, EOI]
    left = 1 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Chunk_0_m = Chunk(input, begin + left, begin + right, self_tagByte);
    let (mut nt_Chunk_0_ipg_start, mut nt_Chunk_0_ipg_end, nt_Chunk_0) = match nt_Chunk_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Chunk_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Chunk_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Chunk_0_ipg_end);
    }
    nt_Chunk_0_ipg_end += left;
    nt_Chunk_0_ipg_start += left;
    left = nt_Chunk_0_ipg_start;
    right = nt_Chunk_0_ipg_end;

    // { chunk = Chunk@0.chunk }
    let mut self_chunk = nt_Chunk_0.chunk;

    return Some((self_ipg_start, self_ipg_end, QOIChunk {
      chunk: self_chunk,
      tagByte: self_tagByte,
    }));
  }

  return None;
}

fn Chunk(input: &[u8], begin: usize, end: usize, a_tagByte: u8) -> Option<(usize, usize, Chunk)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte == 254 ]
    if !(a_tagByte == 254) { break '_ipg_alt; }

    // { tag = "rgb" }
    let mut self_tag = vec![114, 103, 98];

    // { r = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_r = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { g = .[1] }
    left = 1 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_g = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { b = .[2] }
    left = 2 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_b = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { chunk = RGB(r, g, b) }
    let mut self_chunk = RGB(self_r, self_g, self_b);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte == 255 ]
    if !(a_tagByte == 255) { break '_ipg_alt; }

    // { tag = "rgba" }
    let mut self_tag = vec![114, 103, 98, 97];

    // { r = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_r = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { g = .[1] }
    left = 1 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_g = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { b = .[2] }
    left = 2 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_b = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { a = .[3] }
    left = 3 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_a = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { chunk = RGBA(r, g, b, a) }
    let mut self_chunk = RGBA(self_r, self_g, self_b, self_a);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte >> 6 == 0 ]
    if !(a_tagByte >> 6 == 0) { break '_ipg_alt; }

    // { tag = "index" }
    let mut self_tag = vec![105, 110, 100, 101, 120];

    // { index = tagByte & 63 }
    let mut self_index = a_tagByte & 63;

    // { chunk = Index(index) }
    let mut self_chunk = Index(self_index);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte >> 6 == 1 ]
    if !(a_tagByte >> 6 == 1) { break '_ipg_alt; }

    // { tag = "diff" }
    let mut self_tag = vec![100, 105, 102, 102];

    // { dr = wrapping_sub(tagByte >> 4 & 3, 2) }
    let mut self_dr = wrapping_sub(a_tagByte >> 4 & 3, 2);

    // { dg = wrapping_sub(tagByte >> 2 & 3, 2) }
    let mut self_dg = wrapping_sub(a_tagByte >> 2 & 3, 2);

    // { db = wrapping_sub(tagByte & 3, 2) }
    let mut self_db = wrapping_sub(a_tagByte & 3, 2);

    // { chunk = Diff(dr, dg, db) }
    let mut self_chunk = Diff(self_dr, self_dg, self_db);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte >> 6 == 2 ]
    if !(a_tagByte >> 6 == 2) { break '_ipg_alt; }

    // { tag = "luma" }
    let mut self_tag = vec![108, 117, 109, 97];

    // { diffGreen = wrapping_sub(tagByte & 63, 32) }
    let mut self_diffGreen = wrapping_sub(a_tagByte & 63, 32);

    // U8@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U8_0_m = U8(input, begin + left, begin + right);
    let (mut nt_U8_0_ipg_start, mut nt_U8_0_ipg_end, nt_U8_0) = match nt_U8_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U8_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U8_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U8_0_ipg_end);
    }
    nt_U8_0_ipg_end += left;
    nt_U8_0_ipg_start += left;
    left = nt_U8_0_ipg_start;
    right = nt_U8_0_ipg_end;

    // { drdg = wrapping_sub(U8@0.value >> 4, 8) }
    let mut self_drdg = wrapping_sub(nt_U8_0.value >> 4, 8);

    // { dbdg = wrapping_sub(U8@0.value & 15, 8) }
    let mut self_dbdg = wrapping_sub(nt_U8_0.value & 15, 8);

    // { chunk = Luma(diffGreen, drdg, dbdg) }
    let mut self_chunk = Luma(self_diffGreen, self_drdg, self_dbdg);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ tagByte >> 6 == 3 ]
    if !(a_tagByte >> 6 == 3) { break '_ipg_alt; }

    // { tag = "run" }
    let mut self_tag = vec![114, 117, 110];

    // { run = (tagByte & 63) + 1 }
    let mut self_run = (a_tagByte & 63) + 1;

    // { chunk = Run(run) }
    let mut self_chunk = Run(self_run);

    return Some((self_ipg_start, self_ipg_end, Chunk {
      chunk: self_chunk,
      tag: self_tag,
    }));
  }

  return None;
}

fn EndMarker(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, EndMarker)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x00\x00\x00\x00\x00\x00\x00\x01"[0, 8]
    left = 0 as usize;
    right = 8 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0, 0, 0, 0, 0, 0, 0, 1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 8;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, EndMarker {
    }));
  }

  return None;
}

fn U8(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, U8)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { value = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_value = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, U8 {
      value: self_value,
    }));
  }

  return None;
}

fn BE_U32(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BE_U32)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bs = *[0, 4] }
    left = 0 as usize;
    right = 4 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bs = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (bs[3] :: Int) | (bs[2] :: Int) << 8 | (bs[1] :: Int) << 16 | (bs[0] :: Int) << 24 }
    let mut self_value = (self_bs[3] as i64) | (self_bs[2] as i64) << 8 | (self_bs[1] as i64) << 16 | (self_bs[0] as i64) << 24;

    return Some((self_ipg_start, self_ipg_end, BE_U32 {
      bs: self_bs,
      value: self_value,
    }));
  }

  return None;
}

use std::fs;

fn main() {
    let input = fs::read("./test/node/samples/1.qoi").unwrap();
    println!("{:?}", QOI(&input, 0, input.len()));
}
