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
function _ipg_startsWith(s, l, r, prefix) {
  if (r - l < prefix.length) return false;
  if (typeof s === 'string') return s.startsWith(prefix, l);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[l + i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
const _ipg_failTreeRoot = { children: [] };
const _ipg_failTreeStack = [_ipg_failTreeRoot];
function GIF(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "GIF",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Header;
    let nt_LogicalScreen;
    let nt_Blocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Header[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Header[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Header = Header(input, begin + left, begin + right);
    if (nt_Header === null) break _ipg_alt;
    if (nt_Header._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Header._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Header._ipg_end);
    }
    nt_Header._ipg_end += left;
    nt_Header._ipg_start += left;
    left = nt_Header._ipg_start;
    right = nt_Header._ipg_end;

    // LogicalScreen[Header.END, EOI]
    left = nt_Header._ipg_end;
    _ipg_failedTerm = { term: "LogicalScreen[Header.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LogicalScreen = LogicalScreen(input, begin + left, begin + right);
    if (nt_LogicalScreen === null) break _ipg_alt;
    if (nt_LogicalScreen._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LogicalScreen._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LogicalScreen._ipg_end);
    }
    nt_LogicalScreen._ipg_end += left;
    nt_LogicalScreen._ipg_start += left;
    left = nt_LogicalScreen._ipg_start;
    right = nt_LogicalScreen._ipg_end;

    // { logicalScreen = LogicalScreen.this }
    self.logicalScreen = (({_ipg_start,_ipg_end,...o}) => o)(nt_LogicalScreen);

    // Blocks[LogicalScreen.END, EOI]
    left = nt_LogicalScreen._ipg_end;
    _ipg_failedTerm = { term: "Blocks[LogicalScreen.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Blocks = Blocks(input, begin + left, begin + right);
    if (nt_Blocks === null) break _ipg_alt;
    if (nt_Blocks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Blocks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Blocks._ipg_end);
    }
    nt_Blocks._ipg_end += left;
    nt_Blocks._ipg_start += left;
    left = nt_Blocks._ipg_start;
    right = nt_Blocks._ipg_end;

    // { blocks = Blocks.values }
    self.blocks = nt_Blocks.values;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Header(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Header",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "GIF89a"[0, 6]
    left = 0;
    right = 6;
    _ipg_failedTerm = { term: "\"GIF89a\"[0, 6]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "GIF89a")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 6;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LogicalScreen(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LogicalScreen",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LogicalScreenDescriptor;
    let nt_MaybeColorTable;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LogicalScreenDescriptor[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LogicalScreenDescriptor[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LogicalScreenDescriptor = LogicalScreenDescriptor(input, begin + left, begin + right);
    if (nt_LogicalScreenDescriptor === null) break _ipg_alt;
    if (nt_LogicalScreenDescriptor._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LogicalScreenDescriptor._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LogicalScreenDescriptor._ipg_end);
    }
    nt_LogicalScreenDescriptor._ipg_end += left;
    nt_LogicalScreenDescriptor._ipg_start += left;
    left = nt_LogicalScreenDescriptor._ipg_start;
    right = nt_LogicalScreenDescriptor._ipg_end;

    // { descriptor = LogicalScreenDescriptor.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_LogicalScreenDescriptor);

    // MaybeColorTable(LogicalScreenDescriptor.hasGlobalColorTable)[LogicalScreenDescriptor.END, LogicalScreenDescriptor.END + 3 * LogicalScreenDescriptor.globalColorTableSize]
    left = nt_LogicalScreenDescriptor._ipg_end;
    _ipg_failedTerm = { term: "MaybeColorTable(LogicalScreenDescriptor.hasGlobalColorTable)[LogicalScreenDescriptor.END, LogicalScreenDescriptor.END + 3 * LogicalScreenDescriptor.globalColorTableSize]", left, right };
    right = nt_LogicalScreenDescriptor._ipg_end + 3 * nt_LogicalScreenDescriptor.globalColorTableSize;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeColorTable = MaybeColorTable(input, begin + left, begin + right, nt_LogicalScreenDescriptor.hasGlobalColorTable);
    if (nt_MaybeColorTable === null) break _ipg_alt;
    if (nt_MaybeColorTable._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeColorTable._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeColorTable._ipg_end);
    }
    nt_MaybeColorTable._ipg_end += left;
    nt_MaybeColorTable._ipg_start += left;
    left = nt_MaybeColorTable._ipg_start;
    right = nt_MaybeColorTable._ipg_end;

    // { globalColorTable = MaybeColorTable.table }
    self.globalColorTable = nt_MaybeColorTable.table;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LogicalScreenDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LogicalScreenDescriptor",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U16[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "U16[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { width = U16.value }
    self.width = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { height = U16.value }
    self.height = nt_U16.value;

    // { packedFields = .[U16.END] }
    left = nt_U16._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ packedFields = .[U16.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { backgroundColorIndex = .[U16.END + 1] }
    left = nt_U16._ipg_end + 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ backgroundColorIndex = .[U16.END + 1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.backgroundColorIndex = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { pixelAspectRation = .[U16.END + 2] }
    left = nt_U16._ipg_end + 2;
    right = left + 1;
    _ipg_failedTerm = { term: "{ pixelAspectRation = .[U16.END + 2] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.pixelAspectRation = input[begin + left];
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

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function MaybeColorTable(input, begin = 0, end = input.length, a_hasColorTable) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "MaybeColorTable",
    args: [a_hasColorTable],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ hasColorTable == 0 ]
    _ipg_failedTerm = { term: "?[ hasColorTable == 0 ]" };
    if (!(a_hasColorTable == 0)) break _ipg_alt;

    // { table = emptyTable() }
    self.table = emptyTable();

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ColorTable;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ColorTable[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "ColorTable[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ColorTable = ColorTable(input, begin + left, begin + right);
    if (nt_ColorTable === null) break _ipg_alt;
    if (nt_ColorTable._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ColorTable._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ColorTable._ipg_end);
    }
    nt_ColorTable._ipg_end += left;
    nt_ColorTable._ipg_start += left;
    left = nt_ColorTable._ipg_start;
    right = nt_ColorTable._ipg_end;

    // { table = ColorTable.table }
    self.table = nt_ColorTable.table;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function ColorTable(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "ColorTable",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RGB;
    let seq_RGB; let seq_RGB_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 3 do RGB[3 * i, 3 * i + 3]
    _ipg_failedTerm = { term: "for i = 0 to EOI / 3 do RGB[3 * i, 3 * i + 3]" };
    nt_RGB = { _ipg_end: right, _ipg_start: left };
    seq_RGB_start = 0;
    loopEnd = EOI / 3;
    seq_RGB = new Array(loopEnd - seq_RGB_start);
    for (self.i = seq_RGB_start; self.i < loopEnd; self.i++) {
      const left = 3 * self.i;
      const right = 3 * self.i + 3;
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = RGB(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RGB._ipg_end = tmp._ipg_end;
      nt_RGB._ipg_start = tmp._ipg_start;
      seq_RGB[self.i - seq_RGB_start] = tmp;
    }
    delete self.i;
    left = nt_RGB._ipg_start;
    right = nt_RGB._ipg_end;

    // { table = RGB.these }
    self.table = seq_RGB.map(({_ipg_start,_ipg_end,...o}) => o);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Blocks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Blocks",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Block;
    let nt_Trailer;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Block[Block.END, EOI].block starting on [0, EOI] until Trailer
    _ipg_failedTerm = { term: "repeat Block[Block.END, EOI].block starting on [0, EOI] until Trailer" };
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_Trailer = Trailer(input, begin + left, begin + right);
      if (nt_Trailer !== null) {
        if (nt_Trailer._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_Trailer._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_Trailer._ipg_end);
        }
        nt_Trailer._ipg_end += left;
        nt_Trailer._ipg_start += left;
        right = nt_Trailer._ipg_end;
        break;
      }
      nt_Block = Block(input, begin + left, begin + right);
      if (nt_Block === null) break _ipg_alt;
      if (nt_Block._ipg_end === 0) throw 'repeat of non-consuming rule: Block';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Block._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Block._ipg_end);
      nt_Block._ipg_end += left;
      nt_Block._ipg_start += left;
      self.values.push(nt_Block.block);
      left = nt_Block._ipg_end;
      right = EOI;
    }

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Block(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Block",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_GraphicBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // GraphicBlock[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "GraphicBlock[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_GraphicBlock = GraphicBlock(input, begin + left, begin + right);
    if (nt_GraphicBlock === null) break _ipg_alt;
    if (nt_GraphicBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_GraphicBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_GraphicBlock._ipg_end);
    }
    nt_GraphicBlock._ipg_end += left;
    nt_GraphicBlock._ipg_start += left;
    left = nt_GraphicBlock._ipg_start;
    right = nt_GraphicBlock._ipg_end;

    // { block = GraphicBlock.block }
    self.block = nt_GraphicBlock.block;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ApplicationExtension;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ApplicationExtension[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "ApplicationExtension[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ApplicationExtension = ApplicationExtension(input, begin + left, begin + right);
    if (nt_ApplicationExtension === null) break _ipg_alt;
    if (nt_ApplicationExtension._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ApplicationExtension._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ApplicationExtension._ipg_end);
    }
    nt_ApplicationExtension._ipg_end += left;
    nt_ApplicationExtension._ipg_start += left;
    left = nt_ApplicationExtension._ipg_start;
    right = nt_ApplicationExtension._ipg_end;

    // { block = ApplicationExtension.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_ApplicationExtension);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_CommentExtension;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // CommentExtension[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "CommentExtension[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CommentExtension = CommentExtension(input, begin + left, begin + right);
    if (nt_CommentExtension === null) break _ipg_alt;
    if (nt_CommentExtension._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CommentExtension._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CommentExtension._ipg_end);
    }
    nt_CommentExtension._ipg_end += left;
    nt_CommentExtension._ipg_start += left;
    left = nt_CommentExtension._ipg_start;
    right = nt_CommentExtension._ipg_end;

    // { block = CommentExtension.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_CommentExtension);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function GraphicBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "GraphicBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MaybeGraphicControlExtension;
    let nt_GraphicRenderingBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // MaybeGraphicControlExtension[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "MaybeGraphicControlExtension[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeGraphicControlExtension = MaybeGraphicControlExtension(input, begin + left, begin + right);
    if (nt_MaybeGraphicControlExtension === null) break _ipg_alt;
    if (nt_MaybeGraphicControlExtension._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeGraphicControlExtension._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeGraphicControlExtension._ipg_end);
    }
    nt_MaybeGraphicControlExtension._ipg_end += left;
    nt_MaybeGraphicControlExtension._ipg_start += left;
    left = nt_MaybeGraphicControlExtension._ipg_start;
    right = nt_MaybeGraphicControlExtension._ipg_end;

    // GraphicRenderingBlock[MaybeGraphicControlExtension.END, EOI]
    left = nt_MaybeGraphicControlExtension._ipg_end;
    _ipg_failedTerm = { term: "GraphicRenderingBlock[MaybeGraphicControlExtension.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_GraphicRenderingBlock = GraphicRenderingBlock(input, begin + left, begin + right);
    if (nt_GraphicRenderingBlock === null) break _ipg_alt;
    if (nt_GraphicRenderingBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_GraphicRenderingBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_GraphicRenderingBlock._ipg_end);
    }
    nt_GraphicRenderingBlock._ipg_end += left;
    nt_GraphicRenderingBlock._ipg_start += left;
    left = nt_GraphicRenderingBlock._ipg_start;
    right = nt_GraphicRenderingBlock._ipg_end;

    // { block = makeGraphicBlock(MaybeGraphicControlExtension.extension, GraphicRenderingBlock.block) }
    self.block = makeGraphicBlock(nt_MaybeGraphicControlExtension.extension, nt_GraphicRenderingBlock.block);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function MaybeGraphicControlExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "MaybeGraphicControlExtension",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    let nt_BlockTerminator;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xf9"[0, 2]
    left = 0;
    right = 2;
    _ipg_failedTerm = { term: "\"!\\xf9\"[0, 2]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "!\xf9")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 2;

    // "\x04"[2, 3]
    left = 2;
    right = 3;
    _ipg_failedTerm = { term: "\"\\x04\"[2, 3]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x04")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { packedFields = .[3] }
    left = 3;
    right = left + 1;
    _ipg_failedTerm = { term: "{ packedFields = .[3] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { disposalMethod = packedFields >> 2 & 7 }
    self.disposalMethod = self.packedFields >> 2 & 7;

    // { userInputFlag = packedFields >> 1 & 1 }
    self.userInputFlag = self.packedFields >> 1 & 1;

    // { transparentColorFlag = packedFields & 1 }
    self.transparentColorFlag = self.packedFields & 1;

    // U16[4, EOI]
    left = 4;
    _ipg_failedTerm = { term: "U16[4, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { delayTime = U16.value }
    self.delayTime = nt_U16.value;

    // { transparentColorIndex = .[U16.END] }
    left = nt_U16._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ transparentColorIndex = .[U16.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.transparentColorIndex = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BlockTerminator[U16.END + 1, EOI]
    left = nt_U16._ipg_end + 1;
    _ipg_failedTerm = { term: "BlockTerminator[U16.END + 1, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BlockTerminator = BlockTerminator(input, begin + left, begin + right);
    if (nt_BlockTerminator === null) break _ipg_alt;
    if (nt_BlockTerminator._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BlockTerminator._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BlockTerminator._ipg_end);
    }
    nt_BlockTerminator._ipg_end += left;
    nt_BlockTerminator._ipg_start += left;
    left = nt_BlockTerminator._ipg_start;
    right = nt_BlockTerminator._ipg_end;

    // { extension = makeGraphicControlExtension(disposalMethod, userInputFlag, transparentColorFlag, delayTime, transparentColorIndex) }
    self.extension = makeGraphicControlExtension(self.disposalMethod, self.userInputFlag, self.transparentColorFlag, self.delayTime, self.transparentColorIndex);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { extension = getNull() }
    self.extension = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function GraphicRenderingBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "GraphicRenderingBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TableBasedImage;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TableBasedImage[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TableBasedImage[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TableBasedImage = TableBasedImage(input, begin + left, begin + right);
    if (nt_TableBasedImage === null) break _ipg_alt;
    if (nt_TableBasedImage._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TableBasedImage._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TableBasedImage._ipg_end);
    }
    nt_TableBasedImage._ipg_end += left;
    nt_TableBasedImage._ipg_start += left;
    left = nt_TableBasedImage._ipg_start;
    right = nt_TableBasedImage._ipg_end;

    // { block = TableBasedImage.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_TableBasedImage);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_PlainTextExtension;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // PlainTextExtension[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "PlainTextExtension[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PlainTextExtension = PlainTextExtension(input, begin + left, begin + right);
    if (nt_PlainTextExtension === null) break _ipg_alt;
    if (nt_PlainTextExtension._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PlainTextExtension._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PlainTextExtension._ipg_end);
    }
    nt_PlainTextExtension._ipg_end += left;
    nt_PlainTextExtension._ipg_start += left;
    left = nt_PlainTextExtension._ipg_start;
    right = nt_PlainTextExtension._ipg_end;

    // { block = PlainTextExtension.this }
    self.block = (({_ipg_start,_ipg_end,...o}) => o)(nt_PlainTextExtension);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TableBasedImage(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TableBasedImage",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_ImageDescriptor;
    let nt_MaybeColorTable;
    let nt_ImageData;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { type = "tableBasedImage" }
    self.type = "tableBasedImage";

    // ImageDescriptor[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "ImageDescriptor[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ImageDescriptor = ImageDescriptor(input, begin + left, begin + right);
    if (nt_ImageDescriptor === null) break _ipg_alt;
    if (nt_ImageDescriptor._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ImageDescriptor._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ImageDescriptor._ipg_end);
    }
    nt_ImageDescriptor._ipg_end += left;
    nt_ImageDescriptor._ipg_start += left;
    left = nt_ImageDescriptor._ipg_start;
    right = nt_ImageDescriptor._ipg_end;

    // { descriptor = ImageDescriptor.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_ImageDescriptor);

    // MaybeColorTable(ImageDescriptor.localColorTableFlag)[ImageDescriptor.END, ImageDescriptor.END + 3 * ImageDescriptor.localColorTableSize]
    left = nt_ImageDescriptor._ipg_end;
    _ipg_failedTerm = { term: "MaybeColorTable(ImageDescriptor.localColorTableFlag)[ImageDescriptor.END, ImageDescriptor.END + 3 * ImageDescriptor.localColorTableSize]", left, right };
    right = nt_ImageDescriptor._ipg_end + 3 * nt_ImageDescriptor.localColorTableSize;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MaybeColorTable = MaybeColorTable(input, begin + left, begin + right, nt_ImageDescriptor.localColorTableFlag);
    if (nt_MaybeColorTable === null) break _ipg_alt;
    if (nt_MaybeColorTable._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MaybeColorTable._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MaybeColorTable._ipg_end);
    }
    nt_MaybeColorTable._ipg_end += left;
    nt_MaybeColorTable._ipg_start += left;
    left = nt_MaybeColorTable._ipg_start;
    right = nt_MaybeColorTable._ipg_end;

    // { localColorTable = MaybeColorTable.table }
    self.localColorTable = nt_MaybeColorTable.table;

    // ImageData[MaybeColorTable.END, EOI]
    left = nt_MaybeColorTable._ipg_end;
    _ipg_failedTerm = { term: "ImageData[MaybeColorTable.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ImageData = ImageData(input, begin + left, begin + right);
    if (nt_ImageData === null) break _ipg_alt;
    if (nt_ImageData._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ImageData._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ImageData._ipg_end);
    }
    nt_ImageData._ipg_end += left;
    nt_ImageData._ipg_start += left;
    left = nt_ImageData._ipg_start;
    right = nt_ImageData._ipg_end;

    // { data = ImageData.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_ImageData);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function ImageDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "ImageDescriptor",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ","[0, 1]
    left = 0;
    right = 1;
    _ipg_failedTerm = { term: "\",\"[0, 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, ",")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // U16[1, EOI]
    left = 1;
    _ipg_failedTerm = { term: "U16[1, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { imageLeftPosition = U16.value }
    self.imageLeftPosition = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { imageTopPosition = U16.value }
    self.imageTopPosition = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { imageWidth = U16.value }
    self.imageWidth = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { imageHeight = U16.value }
    self.imageHeight = nt_U16.value;

    // { packedFields = .[U16.END] }
    left = nt_U16._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ packedFields = .[U16.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.packedFields = input[begin + left];
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

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function ImageData(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "ImageData",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { lzwMinimumCodeSize = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ lzwMinimumCodeSize = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lzwMinimumCodeSize = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Subblocks[1, EOI]
    left = 1;
    _ipg_failedTerm = { term: "Subblocks[1, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks = Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks === null) break _ipg_alt;
    if (nt_Subblocks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks._ipg_end);
    }
    nt_Subblocks._ipg_end += left;
    nt_Subblocks._ipg_start += left;
    left = nt_Subblocks._ipg_start;
    right = nt_Subblocks._ipg_end;

    // { imageData = concat(Subblocks.values) }
    self.imageData = concat(nt_Subblocks.values);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function PlainTextExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "PlainTextExtension",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    let nt_Subblocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\x01"[0, 2]
    left = 0;
    right = 2;
    _ipg_failedTerm = { term: "\"!\\x01\"[0, 2]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "!\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 2;

    // "\x03"[2, 3]
    left = 2;
    right = 3;
    _ipg_failedTerm = { term: "\"\\x03\"[2, 3]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x03")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { type = "plainTextExtension" }
    self.type = "plainTextExtension";

    // U16[3, EOI]
    left = 3;
    _ipg_failedTerm = { term: "U16[3, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { textGridLeftPosition = U16.value }
    self.textGridLeftPosition = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { textGridTopPosition = U16.value }
    self.textGridTopPosition = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { textGridWidth = U16.value }
    self.textGridWidth = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
    _ipg_failedTerm = { term: "U16[U16.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16 = U16(input, begin + left, begin + right);
    if (nt_U16 === null) break _ipg_alt;
    if (nt_U16._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16._ipg_end);
    }
    nt_U16._ipg_end += left;
    nt_U16._ipg_start += left;
    left = nt_U16._ipg_start;
    right = nt_U16._ipg_end;

    // { textGridHeight = U16.value }
    self.textGridHeight = nt_U16.value;

    // { characterCellWidth = .[U16.END] }
    left = nt_U16._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ characterCellWidth = .[U16.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.characterCellWidth = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { characterCellHeight = .[U16.END + 1] }
    left = nt_U16._ipg_end + 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ characterCellHeight = .[U16.END + 1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.characterCellHeight = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { textForegroundColorIndex = .[U16.END + 2] }
    left = nt_U16._ipg_end + 2;
    right = left + 1;
    _ipg_failedTerm = { term: "{ textForegroundColorIndex = .[U16.END + 2] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.textForegroundColorIndex = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { textBackgroundColorIndex = .[U16.END + 3] }
    left = nt_U16._ipg_end + 3;
    right = left + 1;
    _ipg_failedTerm = { term: "{ textBackgroundColorIndex = .[U16.END + 3] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.textBackgroundColorIndex = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Subblocks[U16.END + 4, EOI]
    left = nt_U16._ipg_end + 4;
    _ipg_failedTerm = { term: "Subblocks[U16.END + 4, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks = Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks === null) break _ipg_alt;
    if (nt_Subblocks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks._ipg_end);
    }
    nt_Subblocks._ipg_end += left;
    nt_Subblocks._ipg_start += left;
    left = nt_Subblocks._ipg_start;
    right = nt_Subblocks._ipg_end;

    // { plainTextData = concat(Subblocks.values) }
    self.plainTextData = concat(nt_Subblocks.values);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function B(input, begin = 0, end = input.length, a_n) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "B",
    args: [a_n],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = *[0, n] }
    left = 0;
    right = a_n;
    _ipg_failedTerm = { term: "{ value = *[0, n] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.value = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function ApplicationExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "ApplicationExtension",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_B;
    let nt_Subblocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xff"[0, 2]
    left = 0;
    right = 2;
    _ipg_failedTerm = { term: "\"!\\xff\"[0, 2]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "!\xff")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 2;

    // "\x0b"[2, 3]
    left = 2;
    right = 3;
    _ipg_failedTerm = { term: "\"\\x0b\"[2, 3]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x0b")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { type = "applicationExtension" }
    self.type = "applicationExtension";

    // B(8)[3, EOI]
    left = 3;
    _ipg_failedTerm = { term: "B(8)[3, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_B = B(input, begin + left, begin + right, 8);
    if (nt_B === null) break _ipg_alt;
    if (nt_B._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_B._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_B._ipg_end);
    }
    nt_B._ipg_end += left;
    nt_B._ipg_start += left;
    left = nt_B._ipg_start;
    right = nt_B._ipg_end;

    // { applicationIdentifier = decodeAscii(B.value) }
    self.applicationIdentifier = decodeAscii(nt_B.value);

    // B(3)[B.END, EOI]
    left = nt_B._ipg_end;
    _ipg_failedTerm = { term: "B(3)[B.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_B = B(input, begin + left, begin + right, 3);
    if (nt_B === null) break _ipg_alt;
    if (nt_B._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_B._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_B._ipg_end);
    }
    nt_B._ipg_end += left;
    nt_B._ipg_start += left;
    left = nt_B._ipg_start;
    right = nt_B._ipg_end;

    // { applicationAuthenticationCode = decodeAscii(B.value) }
    self.applicationAuthenticationCode = decodeAscii(nt_B.value);

    // Subblocks[B.END, EOI]
    left = nt_B._ipg_end;
    _ipg_failedTerm = { term: "Subblocks[B.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks = Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks === null) break _ipg_alt;
    if (nt_Subblocks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks._ipg_end);
    }
    nt_Subblocks._ipg_end += left;
    nt_Subblocks._ipg_start += left;
    left = nt_Subblocks._ipg_start;
    right = nt_Subblocks._ipg_end;

    // { applicationData = concat(Subblocks.values) }
    self.applicationData = concat(nt_Subblocks.values);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function CommentExtension(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "CommentExtension",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "!\xfe"[0, 2]
    left = 0;
    right = 2;
    _ipg_failedTerm = { term: "\"!\\xfe\"[0, 2]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "!\xfe")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 2;

    // { type = "commentExtension" }
    self.type = "commentExtension";

    // Subblocks[2, EOI]
    left = 2;
    _ipg_failedTerm = { term: "Subblocks[2, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Subblocks = Subblocks(input, begin + left, begin + right);
    if (nt_Subblocks === null) break _ipg_alt;
    if (nt_Subblocks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblocks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblocks._ipg_end);
    }
    nt_Subblocks._ipg_end += left;
    nt_Subblocks._ipg_start += left;
    left = nt_Subblocks._ipg_start;
    right = nt_Subblocks._ipg_end;

    // { commentData = decodeAscii(concat(Subblocks.values)) }
    self.commentData = decodeAscii(concat(nt_Subblocks.values));

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Subblocks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Subblocks",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Subblock;
    let nt_BlockTerminator;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Subblock[Subblock.END, EOI].data starting on [0, EOI] until BlockTerminator
    _ipg_failedTerm = { term: "repeat Subblock[Subblock.END, EOI].data starting on [0, EOI] until BlockTerminator" };
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_BlockTerminator = BlockTerminator(input, begin + left, begin + right);
      if (nt_BlockTerminator !== null) {
        if (nt_BlockTerminator._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_BlockTerminator._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_BlockTerminator._ipg_end);
        }
        nt_BlockTerminator._ipg_end += left;
        nt_BlockTerminator._ipg_start += left;
        right = nt_BlockTerminator._ipg_end;
        break;
      }
      nt_Subblock = Subblock(input, begin + left, begin + right);
      if (nt_Subblock === null) break _ipg_alt;
      if (nt_Subblock._ipg_end === 0) throw 'repeat of non-consuming rule: Subblock';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Subblock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Subblock._ipg_end);
      nt_Subblock._ipg_end += left;
      nt_Subblock._ipg_start += left;
      self.values.push(nt_Subblock.data);
      left = nt_Subblock._ipg_end;
      right = EOI;
    }

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Subblock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Subblock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { size = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ size = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.size = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { data = *[1, 1 + size] }
    left = 1;
    right = 1 + self.size;
    _ipg_failedTerm = { term: "{ data = *[1, 1 + size] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.data = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Trailer(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Trailer",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ";"[0, 1]
    left = 0;
    right = 1;
    _ipg_failedTerm = { term: "\";\"[0, 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, ";")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "U16",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 2] }
    left = 0;
    right = 2;
    _ipg_failedTerm = { term: "{ bs = *[0, 2] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = bs[0] | bs[1] << 8 }
    self.value = self.bs[0] | self.bs[1] << 8;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function BlockTerminator(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "BlockTerminator",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00"[0, 1]
    left = 0;
    right = 1;
    _ipg_failedTerm = { term: "\"\\x00\"[0, 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function RGB(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "RGB",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { r = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ r = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.r = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { g = .[1] }
    left = 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ g = .[1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.g = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { b = .[2] }
    left = 2;
    right = left + 1;
    _ipg_failedTerm = { term: "{ b = .[2] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.b = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

console.log(JSON.stringify(GIF(fs.readFileSync("./gif_samples/1.gif"))));
