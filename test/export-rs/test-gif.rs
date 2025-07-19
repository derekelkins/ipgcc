#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unused_variables)]

#[derive(Clone, Debug)]
enum Extension {
    NoExt(),
    GraphicControlExt(u8, bool, bool, i64, u8),
    ApplicationExt(String, String, Vec<u8>),
    CommentExt(String),
    TableBasedImageExt(ImageDescriptor, Vec<Color>, ImageData),
    PlainTextExt(i64, i64, i64, i64, u8, u8, u8, u8, Vec<u8>),
    GraphicBlockExt(Box<Extension>, Box<Extension>),
}

use Extension::*;

fn GBExt(extension: Extension, renderingBlock: Extension) -> Extension {
    GraphicBlockExt(Box::new(extension), Box::new(renderingBlock))
}

fn emptyTable() -> Vec<Color> { vec![] }
fn concat(chunks: &[Vec<u8>]) -> Vec<u8> { chunks.concat() }
fn decodeAscii(bytes: &[u8]) -> String { String::from(std::str::from_utf8(bytes).unwrap()) }
#[derive(Clone, Debug)]
struct Color {
  b: u8,
  g: u8,
  r: u8,
}

#[derive(Clone, Debug)]
struct ExtensionBlock {
  extension: Extension,
}

#[derive(Clone, Debug)]
struct MaybeColorTable {
  table: Vec<Color>,
}

#[derive(Clone, Debug)]
struct GIF {
  blocks: Vec<Extension>,
  logicalScreen: LogicalScreen,
}

#[derive(Clone, Debug)]
struct Header {
}

#[derive(Clone, Debug)]
struct LogicalScreen {
  descriptor: LogicalScreenDescriptor,
  globalColorTable: Vec<Color>,
}

#[derive(Clone, Debug)]
struct LogicalScreenDescriptor {
  backgroundColorIndex: u8,
  colorResolution: u8,
  globalColorTableSize: i64,
  hasGlobalColorTable: bool,
  height: i64,
  packedFields: u8,
  pixelAspectRation: u8,
  sorted: u8,
  width: i64,
}

#[derive(Clone, Debug)]
struct ColorTable {
  table: Vec<Color>,
}

#[derive(Clone, Debug)]
struct Blocks {
  values: Vec<Extension>,
}

#[derive(Clone, Debug)]
struct GraphicBlock {
  extension: Extension,
}

#[derive(Clone, Debug)]
struct MaybeGraphicControlExtension {
  extension: Extension,
}

#[derive(Clone, Debug)]
struct GraphicRenderingBlock {
  extension: Extension,
}

#[derive(Clone, Debug)]
struct TableBasedImage {
  extension: Extension,
}

#[derive(Clone, Debug)]
struct ImageDescriptor {
  imageHeight: i64,
  imageLeftPosition: i64,
  imageTopPosition: i64,
  imageWidth: i64,
  interlaceFlag: bool,
  localColorTableFlag: bool,
  localColorTableSize: i64,
  packedFields: u8,
  sorted: bool,
}

#[derive(Clone, Debug)]
struct ImageData {
  imageData: Vec<u8>,
  lzwMinimumCodeSize: u8,
}

#[derive(Clone, Debug)]
struct PlainTextExtension {
  characterCellHeight: u8,
  characterCellWidth: u8,
  extension: Extension,
  textBackgroundColorIndex: u8,
  textForegroundColorIndex: u8,
  textGridHeight: i64,
  textGridLeftPosition: i64,
  textGridTopPosition: i64,
  textGridWidth: i64,
}

#[derive(Clone, Debug)]
struct B {
  value: Vec<u8>,
}

#[derive(Clone, Debug)]
struct Subblocks {
  values: Vec<Vec<u8>>,
}

#[derive(Clone, Debug)]
struct Subblock {
  data: Vec<u8>,
  size: u8,
}

#[derive(Clone, Debug)]
struct Trailer {
}

#[derive(Clone, Debug)]
struct U16 {
  bs: Vec<u8>,
  value: i64,
}

#[derive(Clone, Debug)]
struct BlockTerminator {
}

fn GIF(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, GIF)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // Header@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Header_0_m = Header(input, begin + left, begin + right);
    let (mut nt_Header_0_ipg_start, mut nt_Header_0_ipg_end, nt_Header_0) = match nt_Header_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Header_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Header_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Header_0_ipg_end);
    }
    nt_Header_0_ipg_end += left;
    nt_Header_0_ipg_start += left;
    left = nt_Header_0_ipg_start;
    right = nt_Header_0_ipg_end;

    // LogicalScreen@0[Header@0.END, EOI]
    left = nt_Header_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LogicalScreen_0_m = LogicalScreen(input, begin + left, begin + right);
    let (mut nt_LogicalScreen_0_ipg_start, mut nt_LogicalScreen_0_ipg_end, nt_LogicalScreen_0) = match nt_LogicalScreen_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LogicalScreen_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LogicalScreen_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LogicalScreen_0_ipg_end);
    }
    nt_LogicalScreen_0_ipg_end += left;
    nt_LogicalScreen_0_ipg_start += left;
    left = nt_LogicalScreen_0_ipg_start;
    right = nt_LogicalScreen_0_ipg_end;

    // { logicalScreen = LogicalScreen@0.this }
    let mut self_logicalScreen = nt_LogicalScreen_0;

    // Blocks@0[LogicalScreen@0.END, EOI]
    left = nt_LogicalScreen_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Blocks_0_m = Blocks(input, begin + left, begin + right);
    let (mut nt_Blocks_0_ipg_start, mut nt_Blocks_0_ipg_end, nt_Blocks_0) = match nt_Blocks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Blocks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Blocks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Blocks_0_ipg_end);
    }
    nt_Blocks_0_ipg_end += left;
    nt_Blocks_0_ipg_start += left;
    left = nt_Blocks_0_ipg_start;
    right = nt_Blocks_0_ipg_end;

    // { blocks = Blocks@0.values }
    let mut self_blocks = nt_Blocks_0.values;

    return Some((self_ipg_start, self_ipg_end, GIF {
      blocks: self_blocks,
      logicalScreen: self_logicalScreen,
    }));
  }

  return None;
}

fn Header(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Header)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "GIF89a"[0, 6]
    left = 0 as usize;
    right = 6 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[71, 73, 70, 56, 57, 97]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 6;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, Header {
    }));
  }

  return None;
}

fn LogicalScreen(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, LogicalScreen)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // LogicalScreenDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LogicalScreenDescriptor_0_m = LogicalScreenDescriptor(input, begin + left, begin + right);
    let (mut nt_LogicalScreenDescriptor_0_ipg_start, mut nt_LogicalScreenDescriptor_0_ipg_end, nt_LogicalScreenDescriptor_0) = match nt_LogicalScreenDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LogicalScreenDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LogicalScreenDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LogicalScreenDescriptor_0_ipg_end);
    }
    nt_LogicalScreenDescriptor_0_ipg_end += left;
    nt_LogicalScreenDescriptor_0_ipg_start += left;
    left = nt_LogicalScreenDescriptor_0_ipg_start;
    right = nt_LogicalScreenDescriptor_0_ipg_end;

    // MaybeColorTable@0(LogicalScreenDescriptor@0.hasGlobalColorTable)[LogicalScreenDescriptor@0.END, LogicalScreenDescriptor@0.END + 3 * LogicalScreenDescriptor@0.globalColorTableSize]
    left = nt_LogicalScreenDescriptor_0_ipg_end as usize;
    right = (nt_LogicalScreenDescriptor_0_ipg_end + (3 * nt_LogicalScreenDescriptor_0.globalColorTableSize) as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_MaybeColorTable_0_m = MaybeColorTable(input, begin + left, begin + right, nt_LogicalScreenDescriptor_0.hasGlobalColorTable);
    let (mut nt_MaybeColorTable_0_ipg_start, mut nt_MaybeColorTable_0_ipg_end, nt_MaybeColorTable_0) = match nt_MaybeColorTable_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_MaybeColorTable_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_MaybeColorTable_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_MaybeColorTable_0_ipg_end);
    }
    nt_MaybeColorTable_0_ipg_end += left;
    nt_MaybeColorTable_0_ipg_start += left;
    left = nt_MaybeColorTable_0_ipg_start;
    right = nt_MaybeColorTable_0_ipg_end;

    // { descriptor = LogicalScreenDescriptor@0.this }
    let mut self_descriptor = nt_LogicalScreenDescriptor_0;

    // { globalColorTable = MaybeColorTable@0.table }
    let mut self_globalColorTable = nt_MaybeColorTable_0.table;

    return Some((self_ipg_start, self_ipg_end, LogicalScreen {
      descriptor: self_descriptor,
      globalColorTable: self_globalColorTable,
    }));
  }

  return None;
}

fn LogicalScreenDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, LogicalScreenDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // U16@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_0_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_0_ipg_start, mut nt_U16_0_ipg_end, nt_U16_0) = match nt_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_0_ipg_end);
    }
    nt_U16_0_ipg_end += left;
    nt_U16_0_ipg_start += left;
    left = nt_U16_0_ipg_start;
    right = nt_U16_0_ipg_end;

    // { width = U16@0.value }
    let mut self_width = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_1_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_1_ipg_start, mut nt_U16_1_ipg_end, nt_U16_1) = match nt_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_1_ipg_end);
    }
    nt_U16_1_ipg_end += left;
    nt_U16_1_ipg_start += left;
    left = nt_U16_1_ipg_start;
    right = nt_U16_1_ipg_end;

    // { height = U16@1.value }
    let mut self_height = nt_U16_1.value;

    // { packedFields = .[U16@1.END] }
    left = nt_U16_1_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_packedFields = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { backgroundColorIndex = .[U16@1.END + 1] }
    left = (nt_U16_1_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_backgroundColorIndex = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { pixelAspectRation = .[U16@1.END + 2] }
    left = (nt_U16_1_ipg_end + 2) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_pixelAspectRation = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { hasGlobalColorTable = packedFields >> 7 == 1 }
    let mut self_hasGlobalColorTable = self_packedFields >> 7 == 1;

    // { colorResolution = (packedFields >> 4 & 7) + 1 }
    let mut self_colorResolution = (self_packedFields >> 4 & 7) + 1;

    // { sorted = packedFields >> 3 & 1 }
    let mut self_sorted = self_packedFields >> 3 & 1;

    // { globalColorTableSize = 2 << (packedFields & 7) }
    let mut self_globalColorTableSize = 2 << (self_packedFields & 7);

    return Some((self_ipg_start, self_ipg_end, LogicalScreenDescriptor {
      backgroundColorIndex: self_backgroundColorIndex,
      colorResolution: self_colorResolution,
      globalColorTableSize: self_globalColorTableSize,
      hasGlobalColorTable: self_hasGlobalColorTable,
      height: self_height,
      packedFields: self_packedFields,
      pixelAspectRation: self_pixelAspectRation,
      sorted: self_sorted,
      width: self_width,
    }));
  }

  return None;
}

fn MaybeColorTable(input: &[u8], begin: usize, end: usize, a_hasColorTable: bool) -> Option<(usize, usize, MaybeColorTable)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ !hasColorTable ]
    if !(!a_hasColorTable) { break '_ipg_alt; }

    // { table = emptyTable() }
    let mut self_table = emptyTable();

    return Some((self_ipg_start, self_ipg_end, MaybeColorTable {
      table: self_table,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ColorTable@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_ColorTable_0_m = ColorTable(input, begin + left, begin + right);
    let (mut nt_ColorTable_0_ipg_start, mut nt_ColorTable_0_ipg_end, nt_ColorTable_0) = match nt_ColorTable_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_ColorTable_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_ColorTable_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_ColorTable_0_ipg_end);
    }
    nt_ColorTable_0_ipg_end += left;
    nt_ColorTable_0_ipg_start += left;
    left = nt_ColorTable_0_ipg_start;
    right = nt_ColorTable_0_ipg_end;

    // { table = ColorTable@0.table }
    let mut self_table = nt_ColorTable_0.table;

    return Some((self_ipg_start, self_ipg_end, MaybeColorTable {
      table: self_table,
    }));
  }

  return None;
}

fn ColorTable(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ColorTable)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // for i = 0 to EOI / 3 do RGB@0[3 * i, 3 * i + 3]
    let mut nt_RGB_0_ipg_start = left;
    let mut nt_RGB_0_ipg_end = right;
    let seq_RGB_0_start = 0 as usize;
    let loopEnd = (EOI / 3) as usize;
    let mut seq_RGB_0 = Vec::with_capacity(loopEnd.saturating_sub(seq_RGB_0_start));
    for i_i in seq_RGB_0_start..loopEnd {
      let left = (3 * i_i) as usize;
      let right = (3 * i_i + 3) as usize;
      if right < left || right > EOI { break '_ipg_alt; }
      let tmp_m = RGB(input, begin + left, begin + right);
      let (mut tmp_ipg_start, mut tmp_ipg_end, tmp) = match tmp_m {
        None => { break '_ipg_alt; }
        Some(p) => p,
      };
      if tmp_ipg_end != 0 {
        self_ipg_start = self_ipg_start.min(left + tmp_ipg_start);
        self_ipg_end = self_ipg_end.max(left + tmp_ipg_end);
      }
      tmp_ipg_end += left;
      tmp_ipg_start += left;
      nt_RGB_0_ipg_end = tmp_ipg_end;
      nt_RGB_0_ipg_start = tmp_ipg_start;
      seq_RGB_0.push(tmp);
    }
    left = nt_RGB_0_ipg_start;
    right = nt_RGB_0_ipg_end;

    // { table = RGB@0.these }
    let mut self_table = seq_RGB_0;

    return Some((self_ipg_start, self_ipg_end, ColorTable {
      table: self_table,
    }));
  }

  return None;
}

fn Blocks(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Blocks)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat Block@0[Block@0.END, EOI].extension starting on [0, EOI] until Trailer@0
    left = 0 as usize;
    right = EOI as usize;
    let mut self_values = Vec::new();
    let mut nt_Trailer_0_ipg_start = right;
    let mut nt_Trailer_0_ipg_end = left;
    loop {
      if right < left || right > EOI { break '_ipg_alt; }
      let nt_Trailer_0_m = Trailer(input, begin + left, begin + right);
      match nt_Trailer_0_m {
        None => {}
        Some((nt_Trailer_0_ipg_start_, nt_Trailer_0_ipg_end_, nt_Trailer_0)) => {
          nt_Trailer_0_ipg_start = nt_Trailer_0_ipg_start_;
          nt_Trailer_0_ipg_end = nt_Trailer_0_ipg_end_;
          if nt_Trailer_0_ipg_end != 0 {
            self_ipg_start = self_ipg_start.min(left + nt_Trailer_0_ipg_start);
            self_ipg_end = self_ipg_end.max(left + nt_Trailer_0_ipg_end);
          }
          nt_Trailer_0_ipg_end += left;
          nt_Trailer_0_ipg_start += left;
          right = nt_Trailer_0_ipg_end;
          break;
        }
      };
      let nt_Block_0_m = Block(input, begin + left, begin + right);
      let (mut nt_Block_0_ipg_start, mut nt_Block_0_ipg_end, nt_Block_0) = match nt_Block_0_m {
        None => { break '_ipg_alt; }
        Some(p) => p,
      };
      if nt_Block_0_ipg_end == 0 { panic!("repeat of non-consuming rule: Block"); }
      self_ipg_start = self_ipg_start.min(left + nt_Block_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Block_0_ipg_end);
      nt_Block_0_ipg_end += left;
      nt_Block_0_ipg_start += left;
      self_values.push(nt_Block_0.extension);
      left = nt_Block_0_ipg_end as usize;
      right = EOI as usize;
    }

    return Some((self_ipg_start, self_ipg_end, Blocks {
      values: self_values,
    }));
  }

  return None;
}

fn Block(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ExtensionBlock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // GraphicBlock@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_GraphicBlock_0_m = GraphicBlock(input, begin + left, begin + right);
    let (mut nt_GraphicBlock_0_ipg_start, mut nt_GraphicBlock_0_ipg_end, nt_GraphicBlock_0) = match nt_GraphicBlock_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_GraphicBlock_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_GraphicBlock_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_GraphicBlock_0_ipg_end);
    }
    nt_GraphicBlock_0_ipg_end += left;
    nt_GraphicBlock_0_ipg_start += left;
    left = nt_GraphicBlock_0_ipg_start;
    right = nt_GraphicBlock_0_ipg_end;

    // { extension = GraphicBlock@0.extension }
    let mut self_extension = nt_GraphicBlock_0.extension;

    return Some((self_ipg_start, self_ipg_end, ExtensionBlock {
      extension: self_extension,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ApplicationExtension@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_ApplicationExtension_0_m = ApplicationExtension(input, begin + left, begin + right);
    let (mut nt_ApplicationExtension_0_ipg_start, mut nt_ApplicationExtension_0_ipg_end, nt_ApplicationExtension_0) = match nt_ApplicationExtension_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_ApplicationExtension_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_ApplicationExtension_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_ApplicationExtension_0_ipg_end);
    }
    nt_ApplicationExtension_0_ipg_end += left;
    nt_ApplicationExtension_0_ipg_start += left;
    left = nt_ApplicationExtension_0_ipg_start;
    right = nt_ApplicationExtension_0_ipg_end;

    // { extension = ApplicationExtension@0.extension }
    let mut self_extension = nt_ApplicationExtension_0.extension;

    return Some((self_ipg_start, self_ipg_end, ExtensionBlock {
      extension: self_extension,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // CommentExtension@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_CommentExtension_0_m = CommentExtension(input, begin + left, begin + right);
    let (mut nt_CommentExtension_0_ipg_start, mut nt_CommentExtension_0_ipg_end, nt_CommentExtension_0) = match nt_CommentExtension_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_CommentExtension_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_CommentExtension_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_CommentExtension_0_ipg_end);
    }
    nt_CommentExtension_0_ipg_end += left;
    nt_CommentExtension_0_ipg_start += left;
    left = nt_CommentExtension_0_ipg_start;
    right = nt_CommentExtension_0_ipg_end;

    // { extension = CommentExtension@0.extension }
    let mut self_extension = nt_CommentExtension_0.extension;

    return Some((self_ipg_start, self_ipg_end, ExtensionBlock {
      extension: self_extension,
    }));
  }

  return None;
}

fn GraphicBlock(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, GraphicBlock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // MaybeGraphicControlExtension@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_MaybeGraphicControlExtension_0_m = MaybeGraphicControlExtension(input, begin + left, begin + right);
    let (mut nt_MaybeGraphicControlExtension_0_ipg_start, mut nt_MaybeGraphicControlExtension_0_ipg_end, nt_MaybeGraphicControlExtension_0) = match nt_MaybeGraphicControlExtension_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_MaybeGraphicControlExtension_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_MaybeGraphicControlExtension_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_MaybeGraphicControlExtension_0_ipg_end);
    }
    nt_MaybeGraphicControlExtension_0_ipg_end += left;
    nt_MaybeGraphicControlExtension_0_ipg_start += left;
    left = nt_MaybeGraphicControlExtension_0_ipg_start;
    right = nt_MaybeGraphicControlExtension_0_ipg_end;

    // GraphicRenderingBlock@0[MaybeGraphicControlExtension@0.END, EOI]
    left = nt_MaybeGraphicControlExtension_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_GraphicRenderingBlock_0_m = GraphicRenderingBlock(input, begin + left, begin + right);
    let (mut nt_GraphicRenderingBlock_0_ipg_start, mut nt_GraphicRenderingBlock_0_ipg_end, nt_GraphicRenderingBlock_0) = match nt_GraphicRenderingBlock_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_GraphicRenderingBlock_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_GraphicRenderingBlock_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_GraphicRenderingBlock_0_ipg_end);
    }
    nt_GraphicRenderingBlock_0_ipg_end += left;
    nt_GraphicRenderingBlock_0_ipg_start += left;
    left = nt_GraphicRenderingBlock_0_ipg_start;
    right = nt_GraphicRenderingBlock_0_ipg_end;

    // { extension = GBExt(MaybeGraphicControlExtension@0.extension, GraphicRenderingBlock@0.extension) }
    let mut self_extension = GBExt(nt_MaybeGraphicControlExtension_0.extension, nt_GraphicRenderingBlock_0.extension);

    return Some((self_ipg_start, self_ipg_end, GraphicBlock {
      extension: self_extension,
    }));
  }

  return None;
}

fn MaybeGraphicControlExtension(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, MaybeGraphicControlExtension)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "!\xf9"[0, 2]
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[33, 249]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 2;
    self_ipg_end = self_ipg_end.max(right);

    // "\x04"[2, 3]
    left = 2 as usize;
    right = 3 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[4]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // { packedFields = .[3] }
    left = 3 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_packedFields = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { disposalMethod = packedFields >> 2 & 7 }
    let mut self_disposalMethod = self_packedFields >> 2 & 7;

    // { userInputFlag = (packedFields >> 1 & 1) == 1 }
    let mut self_userInputFlag = (self_packedFields >> 1 & 1) == 1;

    // { transparentColorFlag = (packedFields & 1) == 1 }
    let mut self_transparentColorFlag = (self_packedFields & 1) == 1;

    // U16@0[4, EOI]
    left = 4 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_0_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_0_ipg_start, mut nt_U16_0_ipg_end, nt_U16_0) = match nt_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_0_ipg_end);
    }
    nt_U16_0_ipg_end += left;
    nt_U16_0_ipg_start += left;
    left = nt_U16_0_ipg_start;
    right = nt_U16_0_ipg_end;

    // { delayTime = U16@0.value }
    let mut self_delayTime = nt_U16_0.value;

    // { transparentColorIndex = .[U16@0.END] }
    left = nt_U16_0_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_transparentColorIndex = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // BlockTerminator@0[U16@0.END + 1, EOI]
    left = (nt_U16_0_ipg_end + 1 as usize) as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BlockTerminator_0_m = BlockTerminator(input, begin + left, begin + right);
    let (mut nt_BlockTerminator_0_ipg_start, mut nt_BlockTerminator_0_ipg_end, nt_BlockTerminator_0) = match nt_BlockTerminator_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BlockTerminator_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BlockTerminator_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BlockTerminator_0_ipg_end);
    }
    nt_BlockTerminator_0_ipg_end += left;
    nt_BlockTerminator_0_ipg_start += left;
    left = nt_BlockTerminator_0_ipg_start;
    right = nt_BlockTerminator_0_ipg_end;

    // { extension = GraphicControlExt(disposalMethod, userInputFlag, transparentColorFlag, delayTime, transparentColorIndex) }
    let mut self_extension = GraphicControlExt(self_disposalMethod, self_userInputFlag, self_transparentColorFlag, self_delayTime, self_transparentColorIndex);

    return Some((self_ipg_start, self_ipg_end, MaybeGraphicControlExtension {
      extension: self_extension,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { extension = NoExt() }
    let mut self_extension = NoExt();

    return Some((self_ipg_start, self_ipg_end, MaybeGraphicControlExtension {
      extension: self_extension,
    }));
  }

  return None;
}

fn GraphicRenderingBlock(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, GraphicRenderingBlock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // TableBasedImage@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_TableBasedImage_0_m = TableBasedImage(input, begin + left, begin + right);
    let (mut nt_TableBasedImage_0_ipg_start, mut nt_TableBasedImage_0_ipg_end, nt_TableBasedImage_0) = match nt_TableBasedImage_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_TableBasedImage_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_TableBasedImage_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_TableBasedImage_0_ipg_end);
    }
    nt_TableBasedImage_0_ipg_end += left;
    nt_TableBasedImage_0_ipg_start += left;
    left = nt_TableBasedImage_0_ipg_start;
    right = nt_TableBasedImage_0_ipg_end;

    // { extension = TableBasedImage@0.extension }
    let mut self_extension = nt_TableBasedImage_0.extension;

    return Some((self_ipg_start, self_ipg_end, GraphicRenderingBlock {
      extension: self_extension,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // PlainTextExtension@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_PlainTextExtension_0_m = PlainTextExtension(input, begin + left, begin + right);
    let (mut nt_PlainTextExtension_0_ipg_start, mut nt_PlainTextExtension_0_ipg_end, nt_PlainTextExtension_0) = match nt_PlainTextExtension_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_PlainTextExtension_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_PlainTextExtension_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_PlainTextExtension_0_ipg_end);
    }
    nt_PlainTextExtension_0_ipg_end += left;
    nt_PlainTextExtension_0_ipg_start += left;
    left = nt_PlainTextExtension_0_ipg_start;
    right = nt_PlainTextExtension_0_ipg_end;

    // { extension = PlainTextExtension@0.extension }
    let mut self_extension = nt_PlainTextExtension_0.extension;

    return Some((self_ipg_start, self_ipg_end, GraphicRenderingBlock {
      extension: self_extension,
    }));
  }

  return None;
}

fn TableBasedImage(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, TableBasedImage)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ImageDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_ImageDescriptor_0_m = ImageDescriptor(input, begin + left, begin + right);
    let (mut nt_ImageDescriptor_0_ipg_start, mut nt_ImageDescriptor_0_ipg_end, nt_ImageDescriptor_0) = match nt_ImageDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_ImageDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_ImageDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_ImageDescriptor_0_ipg_end);
    }
    nt_ImageDescriptor_0_ipg_end += left;
    nt_ImageDescriptor_0_ipg_start += left;
    left = nt_ImageDescriptor_0_ipg_start;
    right = nt_ImageDescriptor_0_ipg_end;

    // MaybeColorTable@0(ImageDescriptor@0.localColorTableFlag)[ImageDescriptor@0.END, ImageDescriptor@0.END + 3 * ImageDescriptor@0.localColorTableSize]
    left = nt_ImageDescriptor_0_ipg_end as usize;
    right = (nt_ImageDescriptor_0_ipg_end + (3 * nt_ImageDescriptor_0.localColorTableSize) as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_MaybeColorTable_0_m = MaybeColorTable(input, begin + left, begin + right, nt_ImageDescriptor_0.localColorTableFlag);
    let (mut nt_MaybeColorTable_0_ipg_start, mut nt_MaybeColorTable_0_ipg_end, nt_MaybeColorTable_0) = match nt_MaybeColorTable_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_MaybeColorTable_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_MaybeColorTable_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_MaybeColorTable_0_ipg_end);
    }
    nt_MaybeColorTable_0_ipg_end += left;
    nt_MaybeColorTable_0_ipg_start += left;
    left = nt_MaybeColorTable_0_ipg_start;
    right = nt_MaybeColorTable_0_ipg_end;

    // ImageData@0[MaybeColorTable@0.END, EOI]
    left = nt_MaybeColorTable_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_ImageData_0_m = ImageData(input, begin + left, begin + right);
    let (mut nt_ImageData_0_ipg_start, mut nt_ImageData_0_ipg_end, nt_ImageData_0) = match nt_ImageData_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_ImageData_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_ImageData_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_ImageData_0_ipg_end);
    }
    nt_ImageData_0_ipg_end += left;
    nt_ImageData_0_ipg_start += left;
    left = nt_ImageData_0_ipg_start;
    right = nt_ImageData_0_ipg_end;

    // { extension = TableBasedImageExt(ImageDescriptor@0.this, MaybeColorTable@0.table, ImageData@0.this) }
    let mut self_extension = TableBasedImageExt(nt_ImageDescriptor_0, nt_MaybeColorTable_0.table, nt_ImageData_0);

    return Some((self_ipg_start, self_ipg_end, TableBasedImage {
      extension: self_extension,
    }));
  }

  return None;
}

fn ImageDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ImageDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ","[0, 1]
    left = 0 as usize;
    right = 1 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[44]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // U16@0[1, EOI]
    left = 1 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_0_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_0_ipg_start, mut nt_U16_0_ipg_end, nt_U16_0) = match nt_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_0_ipg_end);
    }
    nt_U16_0_ipg_end += left;
    nt_U16_0_ipg_start += left;
    left = nt_U16_0_ipg_start;
    right = nt_U16_0_ipg_end;

    // { imageLeftPosition = U16@0.value }
    let mut self_imageLeftPosition = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_1_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_1_ipg_start, mut nt_U16_1_ipg_end, nt_U16_1) = match nt_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_1_ipg_end);
    }
    nt_U16_1_ipg_end += left;
    nt_U16_1_ipg_start += left;
    left = nt_U16_1_ipg_start;
    right = nt_U16_1_ipg_end;

    // { imageTopPosition = U16@1.value }
    let mut self_imageTopPosition = nt_U16_1.value;

    // U16@2[U16@1.END, EOI]
    left = nt_U16_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_2_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_2_ipg_start, mut nt_U16_2_ipg_end, nt_U16_2) = match nt_U16_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_2_ipg_end);
    }
    nt_U16_2_ipg_end += left;
    nt_U16_2_ipg_start += left;
    left = nt_U16_2_ipg_start;
    right = nt_U16_2_ipg_end;

    // { imageWidth = U16@2.value }
    let mut self_imageWidth = nt_U16_2.value;

    // U16@3[U16@2.END, EOI]
    left = nt_U16_2_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_3_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_3_ipg_start, mut nt_U16_3_ipg_end, nt_U16_3) = match nt_U16_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_3_ipg_end);
    }
    nt_U16_3_ipg_end += left;
    nt_U16_3_ipg_start += left;
    left = nt_U16_3_ipg_start;
    right = nt_U16_3_ipg_end;

    // { imageHeight = U16@3.value }
    let mut self_imageHeight = nt_U16_3.value;

    // { packedFields = .[U16@3.END] }
    left = nt_U16_3_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_packedFields = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { localColorTableFlag = packedFields >> 7 == 1 }
    let mut self_localColorTableFlag = self_packedFields >> 7 == 1;

    // { interlaceFlag = (packedFields >> 6 & 1) == 1 }
    let mut self_interlaceFlag = (self_packedFields >> 6 & 1) == 1;

    // { sorted = (packedFields >> 5 & 1) == 1 }
    let mut self_sorted = (self_packedFields >> 5 & 1) == 1;

    // { localColorTableSize = 2 << (packedFields & 7) }
    let mut self_localColorTableSize = 2 << (self_packedFields & 7);

    return Some((self_ipg_start, self_ipg_end, ImageDescriptor {
      imageHeight: self_imageHeight,
      imageLeftPosition: self_imageLeftPosition,
      imageTopPosition: self_imageTopPosition,
      imageWidth: self_imageWidth,
      interlaceFlag: self_interlaceFlag,
      localColorTableFlag: self_localColorTableFlag,
      localColorTableSize: self_localColorTableSize,
      packedFields: self_packedFields,
      sorted: self_sorted,
    }));
  }

  return None;
}

fn ImageData(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ImageData)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { lzwMinimumCodeSize = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_lzwMinimumCodeSize = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // Subblocks@0[1, EOI]
    left = 1 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Subblocks_0_m = Subblocks(input, begin + left, begin + right);
    let (mut nt_Subblocks_0_ipg_start, mut nt_Subblocks_0_ipg_end, nt_Subblocks_0) = match nt_Subblocks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Subblocks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Subblocks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Subblocks_0_ipg_end);
    }
    nt_Subblocks_0_ipg_end += left;
    nt_Subblocks_0_ipg_start += left;
    left = nt_Subblocks_0_ipg_start;
    right = nt_Subblocks_0_ipg_end;

    // { imageData = concat(ref(Subblocks@0.values)) }
    let mut self_imageData = concat(&(nt_Subblocks_0.values));

    return Some((self_ipg_start, self_ipg_end, ImageData {
      imageData: self_imageData,
      lzwMinimumCodeSize: self_lzwMinimumCodeSize,
    }));
  }

  return None;
}

fn PlainTextExtension(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, PlainTextExtension)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "!\x01"[0, 2]
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[33, 1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 2;
    self_ipg_end = self_ipg_end.max(right);

    // "\x03"[2, 3]
    left = 2 as usize;
    right = 3 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[3]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // U16@0[3, EOI]
    left = 3 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_0_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_0_ipg_start, mut nt_U16_0_ipg_end, nt_U16_0) = match nt_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_0_ipg_end);
    }
    nt_U16_0_ipg_end += left;
    nt_U16_0_ipg_start += left;
    left = nt_U16_0_ipg_start;
    right = nt_U16_0_ipg_end;

    // { textGridLeftPosition = U16@0.value }
    let mut self_textGridLeftPosition = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_1_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_1_ipg_start, mut nt_U16_1_ipg_end, nt_U16_1) = match nt_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_1_ipg_end);
    }
    nt_U16_1_ipg_end += left;
    nt_U16_1_ipg_start += left;
    left = nt_U16_1_ipg_start;
    right = nt_U16_1_ipg_end;

    // { textGridTopPosition = U16@1.value }
    let mut self_textGridTopPosition = nt_U16_1.value;

    // U16@2[U16@1.END, EOI]
    left = nt_U16_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_2_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_2_ipg_start, mut nt_U16_2_ipg_end, nt_U16_2) = match nt_U16_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_2_ipg_end);
    }
    nt_U16_2_ipg_end += left;
    nt_U16_2_ipg_start += left;
    left = nt_U16_2_ipg_start;
    right = nt_U16_2_ipg_end;

    // { textGridWidth = U16@2.value }
    let mut self_textGridWidth = nt_U16_2.value;

    // U16@3[U16@2.END, EOI]
    left = nt_U16_2_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_U16_3_m = U16(input, begin + left, begin + right);
    let (mut nt_U16_3_ipg_start, mut nt_U16_3_ipg_end, nt_U16_3) = match nt_U16_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_U16_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_U16_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_U16_3_ipg_end);
    }
    nt_U16_3_ipg_end += left;
    nt_U16_3_ipg_start += left;
    left = nt_U16_3_ipg_start;
    right = nt_U16_3_ipg_end;

    // { textGridHeight = U16@3.value }
    let mut self_textGridHeight = nt_U16_3.value;

    // { characterCellWidth = .[U16@3.END] }
    left = nt_U16_3_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_characterCellWidth = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { characterCellHeight = .[U16@3.END + 1] }
    left = (nt_U16_3_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_characterCellHeight = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { textForegroundColorIndex = .[U16@3.END + 2] }
    left = (nt_U16_3_ipg_end + 2) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_textForegroundColorIndex = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { textBackgroundColorIndex = .[U16@3.END + 3] }
    left = (nt_U16_3_ipg_end + 3) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_textBackgroundColorIndex = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // Subblocks@0[U16@3.END + 4, EOI]
    left = (nt_U16_3_ipg_end + 4 as usize) as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Subblocks_0_m = Subblocks(input, begin + left, begin + right);
    let (mut nt_Subblocks_0_ipg_start, mut nt_Subblocks_0_ipg_end, nt_Subblocks_0) = match nt_Subblocks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Subblocks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Subblocks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Subblocks_0_ipg_end);
    }
    nt_Subblocks_0_ipg_end += left;
    nt_Subblocks_0_ipg_start += left;
    left = nt_Subblocks_0_ipg_start;
    right = nt_Subblocks_0_ipg_end;

    // { extension = PlainTextExt(textGridLeftPosition, textGridTopPosition, textGridWidth, textGridHeight, characterCellWidth, characterCellHeight, textForegroundColorIndex, textBackgroundColorIndex, concat(ref(Subblocks@0.values))) }
    let mut self_extension = PlainTextExt(self_textGridLeftPosition, self_textGridTopPosition, self_textGridWidth, self_textGridHeight, self_characterCellWidth, self_characterCellHeight, self_textForegroundColorIndex, self_textBackgroundColorIndex, concat(&(nt_Subblocks_0.values)));

    return Some((self_ipg_start, self_ipg_end, PlainTextExtension {
      characterCellHeight: self_characterCellHeight,
      characterCellWidth: self_characterCellWidth,
      extension: self_extension,
      textBackgroundColorIndex: self_textBackgroundColorIndex,
      textForegroundColorIndex: self_textForegroundColorIndex,
      textGridHeight: self_textGridHeight,
      textGridLeftPosition: self_textGridLeftPosition,
      textGridTopPosition: self_textGridTopPosition,
      textGridWidth: self_textGridWidth,
    }));
  }

  return None;
}

fn B(input: &[u8], begin: usize, end: usize, a_n: i64) -> Option<(usize, usize, B)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { value = *[0, n] }
    left = 0 as usize;
    right = a_n as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_value = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    return Some((self_ipg_start, self_ipg_end, B {
      value: self_value,
    }));
  }

  return None;
}

fn ApplicationExtension(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ExtensionBlock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "!\xff"[0, 2]
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[33, 255]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 2;
    self_ipg_end = self_ipg_end.max(right);

    // "\x0b"[2, 3]
    left = 2 as usize;
    right = 3 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[11]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // B@0(8)[3, EOI]
    left = 3 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_B_0_m = B(input, begin + left, begin + right, 8);
    let (mut nt_B_0_ipg_start, mut nt_B_0_ipg_end, nt_B_0) = match nt_B_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_B_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_B_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_B_0_ipg_end);
    }
    nt_B_0_ipg_end += left;
    nt_B_0_ipg_start += left;
    left = nt_B_0_ipg_start;
    right = nt_B_0_ipg_end;

    // { applicationIdentifier = decodeAscii(ref(B@0.value)) }
    let mut self_applicationIdentifier = decodeAscii(&(nt_B_0.value));

    // B@1(3)[B@0.END, EOI]
    left = nt_B_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_B_1_m = B(input, begin + left, begin + right, 3);
    let (mut nt_B_1_ipg_start, mut nt_B_1_ipg_end, nt_B_1) = match nt_B_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_B_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_B_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_B_1_ipg_end);
    }
    nt_B_1_ipg_end += left;
    nt_B_1_ipg_start += left;
    left = nt_B_1_ipg_start;
    right = nt_B_1_ipg_end;

    // { applicationAuthenticationCode = decodeAscii(ref(B@1.value)) }
    let mut self_applicationAuthenticationCode = decodeAscii(&(nt_B_1.value));

    // Subblocks@0[B@1.END, EOI]
    left = nt_B_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Subblocks_0_m = Subblocks(input, begin + left, begin + right);
    let (mut nt_Subblocks_0_ipg_start, mut nt_Subblocks_0_ipg_end, nt_Subblocks_0) = match nt_Subblocks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Subblocks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Subblocks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Subblocks_0_ipg_end);
    }
    nt_Subblocks_0_ipg_end += left;
    nt_Subblocks_0_ipg_start += left;
    left = nt_Subblocks_0_ipg_start;
    right = nt_Subblocks_0_ipg_end;

    // { applicationData = concat(ref(Subblocks@0.values)) }
    let mut self_applicationData = concat(&(nt_Subblocks_0.values));

    // { extension = ApplicationExt(applicationIdentifier, applicationAuthenticationCode, applicationData) }
    let mut self_extension = ApplicationExt(self_applicationIdentifier, self_applicationAuthenticationCode, self_applicationData);

    return Some((self_ipg_start, self_ipg_end, ExtensionBlock {
      extension: self_extension,
    }));
  }

  return None;
}

fn CommentExtension(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ExtensionBlock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "!\xfe"[0, 2]
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[33, 254]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 2;
    self_ipg_end = self_ipg_end.max(right);

    // Subblocks@0[2, EOI]
    left = 2 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Subblocks_0_m = Subblocks(input, begin + left, begin + right);
    let (mut nt_Subblocks_0_ipg_start, mut nt_Subblocks_0_ipg_end, nt_Subblocks_0) = match nt_Subblocks_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Subblocks_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Subblocks_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Subblocks_0_ipg_end);
    }
    nt_Subblocks_0_ipg_end += left;
    nt_Subblocks_0_ipg_start += left;
    left = nt_Subblocks_0_ipg_start;
    right = nt_Subblocks_0_ipg_end;

    // { extension = CommentExt(decodeAscii(ref(concat(ref(Subblocks@0.values))))) }
    let mut self_extension = CommentExt(decodeAscii(&(concat(&(nt_Subblocks_0.values)))));

    return Some((self_ipg_start, self_ipg_end, ExtensionBlock {
      extension: self_extension,
    }));
  }

  return None;
}

fn Subblocks(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Subblocks)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat Subblock@0[Subblock@0.END, EOI].data starting on [0, EOI] until BlockTerminator@0
    left = 0 as usize;
    right = EOI as usize;
    let mut self_values = Vec::new();
    let mut nt_BlockTerminator_0_ipg_start = right;
    let mut nt_BlockTerminator_0_ipg_end = left;
    loop {
      if right < left || right > EOI { break '_ipg_alt; }
      let nt_BlockTerminator_0_m = BlockTerminator(input, begin + left, begin + right);
      match nt_BlockTerminator_0_m {
        None => {}
        Some((nt_BlockTerminator_0_ipg_start_, nt_BlockTerminator_0_ipg_end_, nt_BlockTerminator_0)) => {
          nt_BlockTerminator_0_ipg_start = nt_BlockTerminator_0_ipg_start_;
          nt_BlockTerminator_0_ipg_end = nt_BlockTerminator_0_ipg_end_;
          if nt_BlockTerminator_0_ipg_end != 0 {
            self_ipg_start = self_ipg_start.min(left + nt_BlockTerminator_0_ipg_start);
            self_ipg_end = self_ipg_end.max(left + nt_BlockTerminator_0_ipg_end);
          }
          nt_BlockTerminator_0_ipg_end += left;
          nt_BlockTerminator_0_ipg_start += left;
          right = nt_BlockTerminator_0_ipg_end;
          break;
        }
      };
      let nt_Subblock_0_m = Subblock(input, begin + left, begin + right);
      let (mut nt_Subblock_0_ipg_start, mut nt_Subblock_0_ipg_end, nt_Subblock_0) = match nt_Subblock_0_m {
        None => { break '_ipg_alt; }
        Some(p) => p,
      };
      if nt_Subblock_0_ipg_end == 0 { panic!("repeat of non-consuming rule: Subblock"); }
      self_ipg_start = self_ipg_start.min(left + nt_Subblock_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Subblock_0_ipg_end);
      nt_Subblock_0_ipg_end += left;
      nt_Subblock_0_ipg_start += left;
      self_values.push(nt_Subblock_0.data);
      left = nt_Subblock_0_ipg_end as usize;
      right = EOI as usize;
    }

    return Some((self_ipg_start, self_ipg_end, Subblocks {
      values: self_values,
    }));
  }

  return None;
}

fn Subblock(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Subblock)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { size = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_size = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { data = *[1, 1 + size] }
    left = 1 as usize;
    right = (1 + self_size as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_data = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    return Some((self_ipg_start, self_ipg_end, Subblock {
      data: self_data,
      size: self_size,
    }));
  }

  return None;
}

fn Trailer(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Trailer)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ";"[0, 1]
    left = 0 as usize;
    right = 1 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[59]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, Trailer {
    }));
  }

  return None;
}

fn U16(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, U16)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bs = *[0, 2] }
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bs = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (bs[0] :: Int) | (bs[1] :: Int) << 8 }
    let mut self_value = (self_bs[0] as i64) | (self_bs[1] as i64) << 8;

    return Some((self_ipg_start, self_ipg_end, U16 {
      bs: self_bs,
      value: self_value,
    }));
  }

  return None;
}

fn BlockTerminator(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BlockTerminator)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x00"[0, 1]
    left = 0 as usize;
    right = 1 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, BlockTerminator {
    }));
  }

  return None;
}

fn RGB(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Color)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

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

    return Some((self_ipg_start, self_ipg_end, Color {
      b: self_b,
      g: self_g,
      r: self_r,
    }));
  }

  return None;
}

use std::fs;

fn main() {
    let input = fs::read("./test/node/samples/1.gif").unwrap();
    println!("{:#?}", GIF(&input, 0, input.len()));
}
