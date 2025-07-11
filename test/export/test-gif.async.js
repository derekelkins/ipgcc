const fs = require("node:fs");

function emptyTable() {
  return [];
}

function getNull() {
  return null;
}

function concat(chunks) {
  return Buffer.concat(chunks);
}

function decodeAscii(bytes) {
  return new TextDecoder("ascii").decode(new Uint8Array(bytes));
}

function makeGraphicBlock(extension, renderingBlock) {
  return { extension, ...renderingBlock };
}

function makeGraphicControlExtension(
    disposalMethod, userInputFlag, transparentColorFlag, delayTime, transparentColorIndex
) {
  return {
    disposalMethod,
    userInputFlag,
    transparentColorFlag,
    delayTime,
    transparentColorIndex
  };
}
async function _ipg_startsWith(input, l, r, prefix) {
  if (r - l < prefix.length) return false;
  const s = await input.slice(l, l + prefix.length);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
async function GIF(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Header_0;
    let nt_LogicalScreen_0;
    let nt_Blocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Header@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Header_0 = await Header(input, begin + left, begin + right);
    if (nt_Header_0 === null) break _ipg_alt;
    if (nt_Header_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Header_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Header_0._ipg_end);
    }
    nt_Header_0._ipg_end += left;
    nt_Header_0._ipg_start += left;
    left = nt_Header_0._ipg_start;
    right = nt_Header_0._ipg_end;

    // LogicalScreen@0[Header@0.END, EOI]
    left = nt_Header_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LogicalScreen_0 = await LogicalScreen(input, begin + left, begin + right);
    if (nt_LogicalScreen_0 === null) break _ipg_alt;
    if (nt_LogicalScreen_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LogicalScreen_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LogicalScreen_0._ipg_end);
    }
    nt_LogicalScreen_0._ipg_end += left;
    nt_LogicalScreen_0._ipg_start += left;
    left = nt_LogicalScreen_0._ipg_start;
    right = nt_LogicalScreen_0._ipg_end;

    // { logicalScreen = LogicalScreen@0.this }
    self.logicalScreen = (({_ipg_start,_ipg_end,...o}) => o)(nt_LogicalScreen_0);

    // Blocks@0[LogicalScreen@0.END, EOI]
    left = nt_LogicalScreen_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Blocks_0 = await Blocks(input, begin + left, begin + right);
    if (nt_Blocks_0 === null) break _ipg_alt;
    if (nt_Blocks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Blocks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Blocks_0._ipg_end);
    }
    nt_Blocks_0._ipg_end += left;
    nt_Blocks_0._ipg_start += left;
    left = nt_Blocks_0._ipg_start;
    right = nt_Blocks_0._ipg_end;

    // { blocks = Blocks@0.values }
    self.blocks = nt_Blocks_0.values;

    return self;
  }

  
  return null;
}

async function Header(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "GIF89a"[0, 6]
    left = 0;
    right = 6;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "GIF89a")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 6;
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function LogicalScreen(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LogicalScreenDescriptor_0;
    let nt_MaybeColorTable_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LogicalScreenDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LogicalScreenDescriptor_0 = await LogicalScreenDescriptor(input, begin + left, begin + right);
    if (nt_LogicalScreenDescriptor_0 === null) break _ipg_alt;
    if (nt_LogicalScreenDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LogicalScreenDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LogicalScreenDescriptor_0._ipg_end);
    }
    nt_LogicalScreenDescriptor_0._ipg_end += left;
    nt_LogicalScreenDescriptor_0._ipg_start += left;
    left = nt_LogicalScreenDescriptor_0._ipg_start;
    right = nt_LogicalScreenDescriptor_0._ipg_end;

    // { descriptor = LogicalScreenDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_LogicalScreenDescriptor_0);

    // MaybeColorTable@0(LogicalScreenDescriptor@0.hasGlobalColorTable)[LogicalScreenDescriptor@0.END, LogicalScreenDescriptor@0.END + 3 * LogicalScreenDescriptor@0.globalColorTableSize]
    left = nt_LogicalScreenDescriptor_0._ipg_end;
    right = nt_LogicalScreenDescriptor_0._ipg_end + 3 * nt_LogicalScreenDescriptor_0.globalColorTableSize;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeColorTable_0 = await MaybeColorTable(input, begin + left, begin + right, nt_LogicalScreenDescriptor_0.hasGlobalColorTable);
    if (nt_MaybeColorTable_0 === null) break _ipg_alt;
    if (nt_MaybeColorTable_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeColorTable_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeColorTable_0._ipg_end);
    }
    nt_MaybeColorTable_0._ipg_end += left;
    nt_MaybeColorTable_0._ipg_start += left;
    left = nt_MaybeColorTable_0._ipg_start;
    right = nt_MaybeColorTable_0._ipg_end;

    // { globalColorTable = MaybeColorTable@0.table }
    self.globalColorTable = nt_MaybeColorTable_0.table;

    return self;
  }

  
  return null;
}

async function LogicalScreenDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16_0;
    let nt_U16_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U16@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = await U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { width = U16@0.value }
    self.width = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_1 = await U16(input, begin + left, begin + right);
    if (nt_U16_1 === null) break _ipg_alt;
    if (nt_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_1._ipg_end);
    }
    nt_U16_1._ipg_end += left;
    nt_U16_1._ipg_start += left;
    left = nt_U16_1._ipg_start;
    right = nt_U16_1._ipg_end;

    // { height = U16@1.value }
    self.height = nt_U16_1.value;

    // { packedFields = .[U16@1.END] }
    left = nt_U16_1._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { backgroundColorIndex = .[U16@1.END + 1] }
    left = nt_U16_1._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.backgroundColorIndex = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { pixelAspectRation = .[U16@1.END + 2] }
    left = nt_U16_1._ipg_end + 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.pixelAspectRation = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { hasGlobalColorTable = packedFields >> 7 }
    self.hasGlobalColorTable = self.packedFields >> 7;

    // { colorResolution = (packedFields >> 4 & 7) + 1 }
    self.colorResolution = (self.packedFields >> 4 & 7) + 1;

    // { sorted = packedFields >> 3 & 1 }
    self.sorted = self.packedFields >> 3 & 1;

    // { globalColorTableSize = 2 << (packedFields & 7) }
    self.globalColorTableSize = 2 << (self.packedFields & 7);

    return self;
  }

  
  return null;
}

async function MaybeColorTable(input, begin = 0, end = input.length, a_hasColorTable) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ hasColorTable == 0 ]
    if (!(a_hasColorTable == 0)) break _ipg_alt;

    // { table = emptyTable() }
    self.table = emptyTable();

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ColorTable_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ColorTable@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ColorTable_0 = await ColorTable(input, begin + left, begin + right);
    if (nt_ColorTable_0 === null) break _ipg_alt;
    if (nt_ColorTable_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ColorTable_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ColorTable_0._ipg_end);
    }
    nt_ColorTable_0._ipg_end += left;
    nt_ColorTable_0._ipg_start += left;
    left = nt_ColorTable_0._ipg_start;
    right = nt_ColorTable_0._ipg_end;

    // { table = ColorTable@0.table }
    self.table = nt_ColorTable_0.table;

    return self;
  }

  
  return null;
}

async function ColorTable(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RGB_0;
    let seq_RGB_0; let seq_RGB_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 3 do RGB@0[3 * i, 3 * i + 3]
    nt_RGB_0 = { _ipg_end: right, _ipg_start: left };
    seq_RGB_0_start = 0;
    loopEnd = EOI / 3;
    seq_RGB_0 = new Array(Math.max(0, loopEnd - seq_RGB_0_start));
    for (let i_i = seq_RGB_0_start; i_i < loopEnd; i_i++) {
      const left = 3 * i_i;
      const right = 3 * i_i + 3;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = await RGB(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RGB_0._ipg_end = tmp._ipg_end;
      nt_RGB_0._ipg_start = tmp._ipg_start;
      seq_RGB_0[i_i - seq_RGB_0_start] = tmp;
    }
    left = nt_RGB_0._ipg_start;
    right = nt_RGB_0._ipg_end;

    // { table = RGB@0.these }
    self.table = seq_RGB_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

async function Blocks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Block_0;
    let nt_Trailer_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Block@0[Block@0.END, EOI].block starting on [0, EOI] until Trailer@0
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_Trailer_0 = await Trailer(input, begin + left, begin + right);
      if (nt_Trailer_0 !== null) {
        if (nt_Trailer_0._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_Trailer_0._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_Trailer_0._ipg_end);
        }
        nt_Trailer_0._ipg_end += left;
        nt_Trailer_0._ipg_start += left;
        right = nt_Trailer_0._ipg_end;
        break;
      }
      nt_Block_0 = await Block(input, begin + left, begin + right);
      if (nt_Block_0 === null) break _ipg_alt;
      if (nt_Block_0._ipg_end === 0) throw 'repeat of non-consuming rule: Block';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Block_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Block_0._ipg_end);
      nt_Block_0._ipg_end += left;
      nt_Block_0._ipg_start += left;
      self.values.push(nt_Block_0.block);
      left = nt_Block_0._ipg_end;
      right = EOI;
    }

    return self;
  }

  
  return null;
}

async function Block(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_GraphicBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // GraphicBlock@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_GraphicBlock_0 = await GraphicBlock(input, begin + left, begin + right);
    if (nt_GraphicBlock_0 === null) break _ipg_alt;
    if (nt_GraphicBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_GraphicBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_GraphicBlock_0._ipg_end);
    }
    nt_GraphicBlock_0._ipg_end += left;
    nt_GraphicBlock_0._ipg_start += left;
    left = nt_GraphicBlock_0._ipg_start;
    right = nt_GraphicBlock_0._ipg_end;

    // { block = GraphicBlock@0.block }
    self.block = nt_GraphicBlock_0.block;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ApplicationExtension_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ApplicationExtension@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ApplicationExtension_0 = await ApplicationExtension(input, begin + left, begin + right);
    if (nt_ApplicationExtension_0 === null) break _ipg_alt;
    if (nt_ApplicationExtension_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ApplicationExtension_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ApplicationExtension_0._ipg_end);
    }
    nt_ApplicationExtension_0._ipg_end += left;
    nt_ApplicationExtension_0._ipg_start += left;
    left = nt_ApplicationExtension_0._ipg_start;
    right = nt_ApplicationExtension_0._ipg_end;

    // { block = ApplicationExtension@0.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_ApplicationExtension_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_CommentExtension_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // CommentExtension@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CommentExtension_0 = await CommentExtension(input, begin + left, begin + right);
    if (nt_CommentExtension_0 === null) break _ipg_alt;
    if (nt_CommentExtension_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CommentExtension_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CommentExtension_0._ipg_end);
    }
    nt_CommentExtension_0._ipg_end += left;
    nt_CommentExtension_0._ipg_start += left;
    left = nt_CommentExtension_0._ipg_start;
    right = nt_CommentExtension_0._ipg_end;

    // { block = CommentExtension@0.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_CommentExtension_0);

    return self;
  }

  
  return null;
}

async function GraphicBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MaybeGraphicControlExtension_0;
    let nt_GraphicRenderingBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // MaybeGraphicControlExtension@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeGraphicControlExtension_0 = await MaybeGraphicControlExtension(input, begin + left, begin + right);
    if (nt_MaybeGraphicControlExtension_0 === null) break _ipg_alt;
    if (nt_MaybeGraphicControlExtension_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeGraphicControlExtension_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeGraphicControlExtension_0._ipg_end);
    }
    nt_MaybeGraphicControlExtension_0._ipg_end += left;
    nt_MaybeGraphicControlExtension_0._ipg_start += left;
    left = nt_MaybeGraphicControlExtension_0._ipg_start;
    right = nt_MaybeGraphicControlExtension_0._ipg_end;

    // GraphicRenderingBlock@0[MaybeGraphicControlExtension@0.END, EOI]
    left = nt_MaybeGraphicControlExtension_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_GraphicRenderingBlock_0 = await GraphicRenderingBlock(input, begin + left, begin + right);
    if (nt_GraphicRenderingBlock_0 === null) break _ipg_alt;
    if (nt_GraphicRenderingBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_GraphicRenderingBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_GraphicRenderingBlock_0._ipg_end);
    }
    nt_GraphicRenderingBlock_0._ipg_end += left;
    nt_GraphicRenderingBlock_0._ipg_start += left;
    left = nt_GraphicRenderingBlock_0._ipg_start;
    right = nt_GraphicRenderingBlock_0._ipg_end;

    // { block = makeGraphicBlock(MaybeGraphicControlExtension@0.extension, GraphicRenderingBlock@0.block) }
    self.block = makeGraphicBlock(nt_MaybeGraphicControlExtension_0.extension, nt_GraphicRenderingBlock_0.block);

    return self;
  }

  
  return null;
}

async function MaybeGraphicControlExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16_0;
    let nt_BlockTerminator_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xf9"[0, 2]
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "!\xf9")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 2;
    self._ipg_end = Math.max(self._ipg_end, right);

    // "\x04"[2, 3]
    left = 2;
    right = 3;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x04")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { packedFields = .[3] }
    left = 3;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { disposalMethod = packedFields >> 2 & 7 }
    self.disposalMethod = self.packedFields >> 2 & 7;

    // { userInputFlag = packedFields >> 1 & 1 }
    self.userInputFlag = self.packedFields >> 1 & 1;

    // { transparentColorFlag = packedFields & 1 }
    self.transparentColorFlag = self.packedFields & 1;

    // U16@0[4, EOI]
    left = 4;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = await U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { delayTime = U16@0.value }
    self.delayTime = nt_U16_0.value;

    // { transparentColorIndex = .[U16@0.END] }
    left = nt_U16_0._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.transparentColorIndex = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BlockTerminator@0[U16@0.END + 1, EOI]
    left = nt_U16_0._ipg_end + 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BlockTerminator_0 = await BlockTerminator(input, begin + left, begin + right);
    if (nt_BlockTerminator_0 === null) break _ipg_alt;
    if (nt_BlockTerminator_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BlockTerminator_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BlockTerminator_0._ipg_end);
    }
    nt_BlockTerminator_0._ipg_end += left;
    nt_BlockTerminator_0._ipg_start += left;
    left = nt_BlockTerminator_0._ipg_start;
    right = nt_BlockTerminator_0._ipg_end;

    // { extension = makeGraphicControlExtension(disposalMethod, userInputFlag, transparentColorFlag, delayTime, transparentColorIndex) }
    self.extension = makeGraphicControlExtension(self.disposalMethod, self.userInputFlag, self.transparentColorFlag, self.delayTime, self.transparentColorIndex);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { extension = getNull() }
    self.extension = getNull();

    return self;
  }

  
  return null;
}

async function GraphicRenderingBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TableBasedImage_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TableBasedImage@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TableBasedImage_0 = await TableBasedImage(input, begin + left, begin + right);
    if (nt_TableBasedImage_0 === null) break _ipg_alt;
    if (nt_TableBasedImage_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TableBasedImage_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TableBasedImage_0._ipg_end);
    }
    nt_TableBasedImage_0._ipg_end += left;
    nt_TableBasedImage_0._ipg_start += left;
    left = nt_TableBasedImage_0._ipg_start;
    right = nt_TableBasedImage_0._ipg_end;

    // { block = TableBasedImage@0.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_TableBasedImage_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_PlainTextExtension_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // PlainTextExtension@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PlainTextExtension_0 = await PlainTextExtension(input, begin + left, begin + right);
    if (nt_PlainTextExtension_0 === null) break _ipg_alt;
    if (nt_PlainTextExtension_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PlainTextExtension_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PlainTextExtension_0._ipg_end);
    }
    nt_PlainTextExtension_0._ipg_end += left;
    nt_PlainTextExtension_0._ipg_start += left;
    left = nt_PlainTextExtension_0._ipg_start;
    right = nt_PlainTextExtension_0._ipg_end;

    // { block = PlainTextExtension@0.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_PlainTextExtension_0);

    return self;
  }

  
  return null;
}

async function TableBasedImage(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ImageDescriptor_0;
    let nt_MaybeColorTable_0;
    let nt_ImageData_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { type = "tableBasedImage" }
    self.type = "tableBasedImage";

    // ImageDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ImageDescriptor_0 = await ImageDescriptor(input, begin + left, begin + right);
    if (nt_ImageDescriptor_0 === null) break _ipg_alt;
    if (nt_ImageDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ImageDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ImageDescriptor_0._ipg_end);
    }
    nt_ImageDescriptor_0._ipg_end += left;
    nt_ImageDescriptor_0._ipg_start += left;
    left = nt_ImageDescriptor_0._ipg_start;
    right = nt_ImageDescriptor_0._ipg_end;

    // { descriptor = ImageDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_ImageDescriptor_0);

    // MaybeColorTable@0(ImageDescriptor@0.localColorTableFlag)[ImageDescriptor@0.END, ImageDescriptor@0.END + 3 * ImageDescriptor@0.localColorTableSize]
    left = nt_ImageDescriptor_0._ipg_end;
    right = nt_ImageDescriptor_0._ipg_end + 3 * nt_ImageDescriptor_0.localColorTableSize;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeColorTable_0 = await MaybeColorTable(input, begin + left, begin + right, nt_ImageDescriptor_0.localColorTableFlag);
    if (nt_MaybeColorTable_0 === null) break _ipg_alt;
    if (nt_MaybeColorTable_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeColorTable_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeColorTable_0._ipg_end);
    }
    nt_MaybeColorTable_0._ipg_end += left;
    nt_MaybeColorTable_0._ipg_start += left;
    left = nt_MaybeColorTable_0._ipg_start;
    right = nt_MaybeColorTable_0._ipg_end;

    // { localColorTable = MaybeColorTable@0.table }
    self.localColorTable = nt_MaybeColorTable_0.table;

    // ImageData@0[MaybeColorTable@0.END, EOI]
    left = nt_MaybeColorTable_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ImageData_0 = await ImageData(input, begin + left, begin + right);
    if (nt_ImageData_0 === null) break _ipg_alt;
    if (nt_ImageData_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ImageData_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ImageData_0._ipg_end);
    }
    nt_ImageData_0._ipg_end += left;
    nt_ImageData_0._ipg_start += left;
    left = nt_ImageData_0._ipg_start;
    right = nt_ImageData_0._ipg_end;

    // { data = ImageData@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_ImageData_0);

    return self;
  }

  
  return null;
}

async function ImageDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16_0;
    let nt_U16_1;
    let nt_U16_2;
    let nt_U16_3;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ","[0, 1]
    left = 0;
    right = 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, ",")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // U16@0[1, EOI]
    left = 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = await U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { imageLeftPosition = U16@0.value }
    self.imageLeftPosition = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_1 = await U16(input, begin + left, begin + right);
    if (nt_U16_1 === null) break _ipg_alt;
    if (nt_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_1._ipg_end);
    }
    nt_U16_1._ipg_end += left;
    nt_U16_1._ipg_start += left;
    left = nt_U16_1._ipg_start;
    right = nt_U16_1._ipg_end;

    // { imageTopPosition = U16@1.value }
    self.imageTopPosition = nt_U16_1.value;

    // U16@2[U16@1.END, EOI]
    left = nt_U16_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_2 = await U16(input, begin + left, begin + right);
    if (nt_U16_2 === null) break _ipg_alt;
    if (nt_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_2._ipg_end);
    }
    nt_U16_2._ipg_end += left;
    nt_U16_2._ipg_start += left;
    left = nt_U16_2._ipg_start;
    right = nt_U16_2._ipg_end;

    // { imageWidth = U16@2.value }
    self.imageWidth = nt_U16_2.value;

    // U16@3[U16@2.END, EOI]
    left = nt_U16_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_3 = await U16(input, begin + left, begin + right);
    if (nt_U16_3 === null) break _ipg_alt;
    if (nt_U16_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_3._ipg_end);
    }
    nt_U16_3._ipg_end += left;
    nt_U16_3._ipg_start += left;
    left = nt_U16_3._ipg_start;
    right = nt_U16_3._ipg_end;

    // { imageHeight = U16@3.value }
    self.imageHeight = nt_U16_3.value;

    // { packedFields = .[U16@3.END] }
    left = nt_U16_3._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { localColorTableFlag = packedFields >> 7 }
    self.localColorTableFlag = self.packedFields >> 7;

    // { interlaceFlag = packedFields >> 6 & 1 }
    self.interlaceFlag = self.packedFields >> 6 & 1;

    // { sorted = packedFields >> 5 & 1 }
    self.sorted = self.packedFields >> 5 & 1;

    // { localColorTableSize = 2 << (packedFields & 7) }
    self.localColorTableSize = 2 << (self.packedFields & 7);

    return self;
  }

  
  return null;
}

async function ImageData(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { lzwMinimumCodeSize = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lzwMinimumCodeSize = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Subblocks@0[1, EOI]
    left = 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks_0 = await Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks_0 === null) break _ipg_alt;
    if (nt_Subblocks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks_0._ipg_end);
    }
    nt_Subblocks_0._ipg_end += left;
    nt_Subblocks_0._ipg_start += left;
    left = nt_Subblocks_0._ipg_start;
    right = nt_Subblocks_0._ipg_end;

    // { imageData = concat(Subblocks@0.values) }
    self.imageData = concat(nt_Subblocks_0.values);

    return self;
  }

  
  return null;
}

async function PlainTextExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16_0;
    let nt_U16_1;
    let nt_U16_2;
    let nt_U16_3;
    let nt_Subblocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\x01"[0, 2]
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "!\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 2;
    self._ipg_end = Math.max(self._ipg_end, right);

    // "\x03"[2, 3]
    left = 2;
    right = 3;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x03")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { type = "plainTextExtension" }
    self.type = "plainTextExtension";

    // U16@0[3, EOI]
    left = 3;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = await U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { textGridLeftPosition = U16@0.value }
    self.textGridLeftPosition = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_1 = await U16(input, begin + left, begin + right);
    if (nt_U16_1 === null) break _ipg_alt;
    if (nt_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_1._ipg_end);
    }
    nt_U16_1._ipg_end += left;
    nt_U16_1._ipg_start += left;
    left = nt_U16_1._ipg_start;
    right = nt_U16_1._ipg_end;

    // { textGridTopPosition = U16@1.value }
    self.textGridTopPosition = nt_U16_1.value;

    // U16@2[U16@1.END, EOI]
    left = nt_U16_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_2 = await U16(input, begin + left, begin + right);
    if (nt_U16_2 === null) break _ipg_alt;
    if (nt_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_2._ipg_end);
    }
    nt_U16_2._ipg_end += left;
    nt_U16_2._ipg_start += left;
    left = nt_U16_2._ipg_start;
    right = nt_U16_2._ipg_end;

    // { textGridWidth = U16@2.value }
    self.textGridWidth = nt_U16_2.value;

    // U16@3[U16@2.END, EOI]
    left = nt_U16_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_3 = await U16(input, begin + left, begin + right);
    if (nt_U16_3 === null) break _ipg_alt;
    if (nt_U16_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_3._ipg_end);
    }
    nt_U16_3._ipg_end += left;
    nt_U16_3._ipg_start += left;
    left = nt_U16_3._ipg_start;
    right = nt_U16_3._ipg_end;

    // { textGridHeight = U16@3.value }
    self.textGridHeight = nt_U16_3.value;

    // { characterCellWidth = .[U16@3.END] }
    left = nt_U16_3._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.characterCellWidth = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { characterCellHeight = .[U16@3.END + 1] }
    left = nt_U16_3._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.characterCellHeight = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { textForegroundColorIndex = .[U16@3.END + 2] }
    left = nt_U16_3._ipg_end + 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.textForegroundColorIndex = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { textBackgroundColorIndex = .[U16@3.END + 3] }
    left = nt_U16_3._ipg_end + 3;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.textBackgroundColorIndex = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Subblocks@0[U16@3.END + 4, EOI]
    left = nt_U16_3._ipg_end + 4;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks_0 = await Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks_0 === null) break _ipg_alt;
    if (nt_Subblocks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks_0._ipg_end);
    }
    nt_Subblocks_0._ipg_end += left;
    nt_Subblocks_0._ipg_start += left;
    left = nt_Subblocks_0._ipg_start;
    right = nt_Subblocks_0._ipg_end;

    // { plainTextData = concat(Subblocks@0.values) }
    self.plainTextData = concat(nt_Subblocks_0.values);

    return self;
  }

  
  return null;
}

async function B(input, begin = 0, end = input.length, a_n) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = *[0, n] }
    left = 0;
    right = a_n;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.value = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

async function ApplicationExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_B_0;
    let nt_B_1;
    let nt_Subblocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xff"[0, 2]
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "!\xff")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 2;
    self._ipg_end = Math.max(self._ipg_end, right);

    // "\x0b"[2, 3]
    left = 2;
    right = 3;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x0b")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { type = "applicationExtension" }
    self.type = "applicationExtension";

    // B@0(8)[3, EOI]
    left = 3;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_B_0 = await B(input, begin + left, begin + right, 8);
    if (nt_B_0 === null) break _ipg_alt;
    if (nt_B_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_B_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_B_0._ipg_end);
    }
    nt_B_0._ipg_end += left;
    nt_B_0._ipg_start += left;
    left = nt_B_0._ipg_start;
    right = nt_B_0._ipg_end;

    // { applicationIdentifier = decodeAscii(B@0.value) }
    self.applicationIdentifier = decodeAscii(nt_B_0.value);

    // B@1(3)[B@0.END, EOI]
    left = nt_B_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_B_1 = await B(input, begin + left, begin + right, 3);
    if (nt_B_1 === null) break _ipg_alt;
    if (nt_B_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_B_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_B_1._ipg_end);
    }
    nt_B_1._ipg_end += left;
    nt_B_1._ipg_start += left;
    left = nt_B_1._ipg_start;
    right = nt_B_1._ipg_end;

    // { applicationAuthenticationCode = decodeAscii(B@1.value) }
    self.applicationAuthenticationCode = decodeAscii(nt_B_1.value);

    // Subblocks@0[B@1.END, EOI]
    left = nt_B_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks_0 = await Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks_0 === null) break _ipg_alt;
    if (nt_Subblocks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks_0._ipg_end);
    }
    nt_Subblocks_0._ipg_end += left;
    nt_Subblocks_0._ipg_start += left;
    left = nt_Subblocks_0._ipg_start;
    right = nt_Subblocks_0._ipg_end;

    // { applicationData = concat(Subblocks@0.values) }
    self.applicationData = concat(nt_Subblocks_0.values);

    return self;
  }

  
  return null;
}

async function CommentExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xfe"[0, 2]
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "!\xfe")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 2;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { type = "commentExtension" }
    self.type = "commentExtension";

    // Subblocks@0[2, EOI]
    left = 2;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks_0 = await Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks_0 === null) break _ipg_alt;
    if (nt_Subblocks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks_0._ipg_end);
    }
    nt_Subblocks_0._ipg_end += left;
    nt_Subblocks_0._ipg_start += left;
    left = nt_Subblocks_0._ipg_start;
    right = nt_Subblocks_0._ipg_end;

    // { commentData = decodeAscii(concat(Subblocks@0.values)) }
    self.commentData = decodeAscii(concat(nt_Subblocks_0.values));

    return self;
  }

  
  return null;
}

async function Subblocks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblock_0;
    let nt_BlockTerminator_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Subblock@0[Subblock@0.END, EOI].data starting on [0, EOI] until BlockTerminator@0
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_BlockTerminator_0 = await BlockTerminator(input, begin + left, begin + right);
      if (nt_BlockTerminator_0 !== null) {
        if (nt_BlockTerminator_0._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_BlockTerminator_0._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_BlockTerminator_0._ipg_end);
        }
        nt_BlockTerminator_0._ipg_end += left;
        nt_BlockTerminator_0._ipg_start += left;
        right = nt_BlockTerminator_0._ipg_end;
        break;
      }
      nt_Subblock_0 = await Subblock(input, begin + left, begin + right);
      if (nt_Subblock_0 === null) break _ipg_alt;
      if (nt_Subblock_0._ipg_end === 0) throw 'repeat of non-consuming rule: Subblock';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblock_0._ipg_end);
      nt_Subblock_0._ipg_end += left;
      nt_Subblock_0._ipg_start += left;
      self.values.push(nt_Subblock_0.data);
      left = nt_Subblock_0._ipg_end;
      right = EOI;
    }

    return self;
  }

  
  return null;
}

async function Subblock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { size = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.size = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { data = *[1, 1 + size] }
    left = 1;
    right = 1 + self.size;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.data = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

async function Trailer(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ";"[0, 1]
    left = 0;
    right = 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, ";")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 2] }
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = bs[0] | bs[1] << 8 }
    self.value = self.bs[0] | self.bs[1] << 8;

    return self;
  }

  
  return null;
}

async function BlockTerminator(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00"[0, 1]
    left = 0;
    right = 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function RGB(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { r = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.r = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { g = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.g = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { b = .[2] }
    left = 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.b = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

console.log(JSON.stringify(GIF(fs.readFileSync("./gif_samples/1.gif"))));
