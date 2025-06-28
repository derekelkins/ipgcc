// We have a series of tagged blocks consisting of basic types and small
// structs of these. We also have a Last-Writer-Wins (LWW) CRDT and a
// more complicated, but not *too* sophisticated, sequence CRDT. Sequences
// of these tagged blocks form nodes of a Scene tree.

// The expectedIndexes are like protobuf field IDs. (For all I know they may
// literally *be* protobuf (or similar) field IDs. These expectedIndexes allow
// removing fields since the corresponding "key" won't be there. See
// SceneGlyphItemBlock.

const fs = require("node:fs");

function warnIf(doWarn, message) {
  if (doWarn) {
    console.error(message);
    // console.warn(message);
  }
  return true;
}

function check(b, message) {
  if (!b) {
    throw message;
  }
  return true;
}

function decodeAscii(bytes) {
  return new TextDecoder("ascii").decode(bytes);
}

function getNull() {
  return null;
}

function getLength(s) {
  return s.length;
}

function getTwoPi() {
  return 2*Math.pi;
}

function lwwBool(id, value) {
  return { id, value };
}

function lwwU8(id, value) {
  return { id, value };
}

function lwwFloat32(id, value) {
  return { id, value };
}

function lwwCrdtId(id, value) {
  return { id, value };
}

function lwwString(id, value) {
  return { id, value };
}

function makeCrdtId(part1, part2) {
  return { part1, part2 };
}

function makePair(fst, snd) {
  return { fst, snd };
}

function makeStringWithFormat(s, fmt) {
  return { s, fmt };
}

function processTextItemValue(value) {
  if (value.fmt !== null) {
    return value.fmt;
  }
  return value.s;
}

function makeCrdtSequenceItem(info, value) {
  return { ...info, value };
}

function blockTypeToName(blockType) {
    switch(blockType) {
        case 0x00: return "MigrationInfoBlock";
        case 0x01: return "SceneTreeBlock";
        case 0x02: return "TreeNodeBlock";
        case 0x03: return "SceneGlyphItemBlock";
        case 0x04: return "SceneGroupItemBlock";
        case 0x05: return "SceneLineItemBlock";
        case 0x06: return "SceneTextItemBlock";
        case 0x07: return "RootTextBlock";
        case 0x08: return "SceneTombstoneItemBlock";
        case 0x09: return "AuthorIdsBlock";
        case 0x0A: return "PageInfoBlock";
        case 0x0D: return "SceneInfo";
        default: return "Unknown";
    }
}

function makeBlock(mainBlockInfo, data, extraData) {
  return { ...mainBlockInfo, typeName: blockTypeToName(mainBlockInfo.blockType), data, extraData };
}

function makeSceneGlyItemBlockValue(start, length, colorId, text, rectangles) {
  return { start, length, colorId, text, rectangles };
}

function toFloat32(bs) {
  // return new Float32Array(bs.buffer)[0];
  return new DataView(new Uint8Array(bs).buffer).getFloat32(0, true);
}

function toFloat64(bs) {
  // return new Float64Array(bs.buffer)[0];
  return new DataView(new Uint8Array(bs).buffer).getFloat64(0, true);
}

function pointSerializedSize(version) {
    switch(version) {
        case 1: return 0x18;
        case 2: return 0x0E;
        default: throw `Don't know the pointSerializedSize for version ${version}`;
    }
}

function makeSceneLineItemBlockValue(
  toolId, colorId, thicknessScale, startLength, points, moveId)
{
  return { toolId, colorId, thicknessScale, startLength, points, moveId };
}

function checkRootTextBlockId(blockId) {
  return check(blockId.part1 === 0 && blockId.part2 === 0, "Expect root text block to have RootId");
}
function _ipg_startsWith(s, l, r, prefix) {
  if (r - l < prefix.length) return false;
  if (typeof s === 'string') return s.startsWith(prefix, l);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[l + i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
function RM6(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_HeaderV6;
    let nt_Blocks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // HeaderV6[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HeaderV6 = HeaderV6(input, begin + left, begin + right);
    if (nt_HeaderV6 === null) break _ipg_alt;
    if (nt_HeaderV6._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HeaderV6._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HeaderV6._ipg_end);
    }
    nt_HeaderV6._ipg_end += left;
    nt_HeaderV6._ipg_start += left;
    left = nt_HeaderV6._ipg_start;
    right = nt_HeaderV6._ipg_end;

    // Blocks[HeaderV6.END, EOI]
    left = nt_HeaderV6._ipg_end;
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

    return self;
  }

  
  return null;
}

function HeaderV6(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "reMarkable .lines file, version=6          "[0, 43]
    left = 0;
    right = 43;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "reMarkable .lines file, version=6          ")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 43;

    return self;
  }

  
  return null;
}

function Blocks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_FullBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat FullBlock.block
    self.values = [];
    nt_FullBlock = FullBlock(input, begin + right, begin + EOI);
    if (nt_FullBlock !== null) {
      if (nt_FullBlock._ipg_end === 0) throw 'repeat of non-consuming rule: FullBlock';
      self._ipg_start = Math.min(self._ipg_start, right + nt_FullBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, right + nt_FullBlock._ipg_end);
      nt_FullBlock._ipg_end += right;
      nt_FullBlock._ipg_start += right;
      left = nt_FullBlock._ipg_start;
      right = nt_FullBlock._ipg_end;
      self.values.push(nt_FullBlock.block);

      while (right <= EOI) {
        nt_FullBlock = FullBlock(input, begin + right, begin + EOI);
        if (nt_FullBlock === null) break;
        if (nt_FullBlock._ipg_end === 0) throw 'repeat of non-consuming rule: FullBlock';
        self._ipg_start = Math.min(self._ipg_start, right + nt_FullBlock._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, right + nt_FullBlock._ipg_end);
        nt_FullBlock._ipg_end += right;
        nt_FullBlock._ipg_start += right;
        self.values.push(nt_FullBlock.block);
        right = nt_FullBlock._ipg_end;
      }
    }

    return self;
  }

  
  return null;
}

function FullBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MainBlockInfo;
    let nt_Block;
    let nt_Bytes;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // MainBlockInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MainBlockInfo = MainBlockInfo(input, begin + left, begin + right);
    if (nt_MainBlockInfo === null) break _ipg_alt;
    if (nt_MainBlockInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MainBlockInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MainBlockInfo._ipg_end);
    }
    nt_MainBlockInfo._ipg_end += left;
    nt_MainBlockInfo._ipg_start += left;
    left = nt_MainBlockInfo._ipg_start;
    right = nt_MainBlockInfo._ipg_end;

    // Block(MainBlockInfo.blockType, MainBlockInfo.currentVersion)[MainBlockInfo.END, MainBlockInfo.END + MainBlockInfo.length]
    left = nt_MainBlockInfo._ipg_end;
    right = nt_MainBlockInfo._ipg_end + nt_MainBlockInfo.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Block = Block(input, begin + left, begin + right, nt_MainBlockInfo.blockType, nt_MainBlockInfo.currentVersion);
    if (nt_Block === null) break _ipg_alt;
    if (nt_Block._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Block._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Block._ipg_end);
    }
    nt_Block._ipg_end += left;
    nt_Block._ipg_start += left;
    left = nt_Block._ipg_start;
    right = nt_Block._ipg_end;

    // Bytes[Block.END, MainBlockInfo.END + MainBlockInfo.length]
    left = nt_Block._ipg_end;
    right = nt_MainBlockInfo._ipg_end + nt_MainBlockInfo.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes = Bytes(input, begin + left, begin + right);
    if (nt_Bytes === null) break _ipg_alt;
    if (nt_Bytes._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes._ipg_end);
    }
    nt_Bytes._ipg_end += left;
    nt_Bytes._ipg_start += left;
    left = nt_Bytes._ipg_start;
    right = nt_Bytes._ipg_end;

    // { block = makeBlock(MainBlockInfo.this, Block.data, Bytes.value) }
    self.block = makeBlock((({_ipg_start,_ipg_end,...o}) => o)(nt_MainBlockInfo), nt_Block.data, nt_Bytes.value);

    return self;
  }

  
  return null;
}

function MainBlockInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32;
    let nt_U8;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U32[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32 = U32(input, begin + left, begin + right);
    if (nt_U32 === null) break _ipg_alt;
    if (nt_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32._ipg_end);
    }
    nt_U32._ipg_end += left;
    nt_U32._ipg_start += left;
    left = nt_U32._ipg_start;
    right = nt_U32._ipg_end;

    // { length = U32.value }
    self.length = nt_U32.value;

    // U8[U32.END, EOI]
    left = nt_U32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U8 = U8(input, begin + left, begin + right);
    if (nt_U8 === null) break _ipg_alt;
    if (nt_U8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8._ipg_end);
    }
    nt_U8._ipg_end += left;
    nt_U8._ipg_start += left;
    left = nt_U8._ipg_start;
    right = nt_U8._ipg_end;

    // ?[ check(U8.value == 0, "unknown value is non-zero") ]
    if (!check(nt_U8.value == 0, "unknown value is non-zero")) break _ipg_alt;

    // { minVersion = .[U8.END] }
    left = nt_U8._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.minVersion = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ check(minVersion >= 0, "minVersion < 0") ]
    if (!check(self.minVersion >= 0, "minVersion < 0")) break _ipg_alt;

    // { currentVersion = .[U8.END + 1] }
    left = nt_U8._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.currentVersion = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ check(currentVersion >= 0 && minVersion <= currentVersion, "currentVersion not between 0 and minVersion") ]
    if (!check(self.currentVersion >= 0 && self.minVersion <= self.currentVersion, "currentVersion not between 0 and minVersion")) break _ipg_alt;

    // { blockType = .[U8.END + 2] }
    left = nt_U8._ipg_end + 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.blockType = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

function Block(input, begin = 0, end = input.length, a_blockType, a_version) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MigrationInfoBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 0 ]
    if (!(a_blockType == 0)) break _ipg_alt;

    // MigrationInfoBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MigrationInfoBlock = MigrationInfoBlock(input, begin + left, begin + right);
    if (nt_MigrationInfoBlock === null) break _ipg_alt;
    if (nt_MigrationInfoBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MigrationInfoBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MigrationInfoBlock._ipg_end);
    }
    nt_MigrationInfoBlock._ipg_end += left;
    nt_MigrationInfoBlock._ipg_start += left;
    left = nt_MigrationInfoBlock._ipg_start;
    right = nt_MigrationInfoBlock._ipg_end;

    // { data = MigrationInfoBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_MigrationInfoBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTreeBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 1 ]
    if (!(a_blockType == 1)) break _ipg_alt;

    // SceneTreeBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTreeBlock = SceneTreeBlock(input, begin + left, begin + right);
    if (nt_SceneTreeBlock === null) break _ipg_alt;
    if (nt_SceneTreeBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTreeBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTreeBlock._ipg_end);
    }
    nt_SceneTreeBlock._ipg_end += left;
    nt_SceneTreeBlock._ipg_start += left;
    left = nt_SceneTreeBlock._ipg_start;
    right = nt_SceneTreeBlock._ipg_end;

    // { data = SceneTreeBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTreeBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TreeNodeBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 2 ]
    if (!(a_blockType == 2)) break _ipg_alt;

    // TreeNodeBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TreeNodeBlock = TreeNodeBlock(input, begin + left, begin + right);
    if (nt_TreeNodeBlock === null) break _ipg_alt;
    if (nt_TreeNodeBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TreeNodeBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TreeNodeBlock._ipg_end);
    }
    nt_TreeNodeBlock._ipg_end += left;
    nt_TreeNodeBlock._ipg_start += left;
    left = nt_TreeNodeBlock._ipg_start;
    right = nt_TreeNodeBlock._ipg_end;

    // { data = TreeNodeBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_TreeNodeBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneGlyphItemBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 3 ]
    if (!(a_blockType == 3)) break _ipg_alt;

    // SceneGlyphItemBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGlyphItemBlock = SceneGlyphItemBlock(input, begin + left, begin + right);
    if (nt_SceneGlyphItemBlock === null) break _ipg_alt;
    if (nt_SceneGlyphItemBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGlyphItemBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGlyphItemBlock._ipg_end);
    }
    nt_SceneGlyphItemBlock._ipg_end += left;
    nt_SceneGlyphItemBlock._ipg_start += left;
    left = nt_SceneGlyphItemBlock._ipg_start;
    right = nt_SceneGlyphItemBlock._ipg_end;

    // { data = SceneGlyphItemBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneGlyphItemBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneGroupItemBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 4 ]
    if (!(a_blockType == 4)) break _ipg_alt;

    // SceneGroupItemBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGroupItemBlock = SceneGroupItemBlock(input, begin + left, begin + right);
    if (nt_SceneGroupItemBlock === null) break _ipg_alt;
    if (nt_SceneGroupItemBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGroupItemBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGroupItemBlock._ipg_end);
    }
    nt_SceneGroupItemBlock._ipg_end += left;
    nt_SceneGroupItemBlock._ipg_start += left;
    left = nt_SceneGroupItemBlock._ipg_start;
    right = nt_SceneGroupItemBlock._ipg_end;

    // { data = SceneGroupItemBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneGroupItemBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneLineItemBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 5 ]
    if (!(a_blockType == 5)) break _ipg_alt;

    // SceneLineItemBlock(version)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneLineItemBlock = SceneLineItemBlock(input, begin + left, begin + right, a_version);
    if (nt_SceneLineItemBlock === null) break _ipg_alt;
    if (nt_SceneLineItemBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneLineItemBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneLineItemBlock._ipg_end);
    }
    nt_SceneLineItemBlock._ipg_end += left;
    nt_SceneLineItemBlock._ipg_start += left;
    left = nt_SceneLineItemBlock._ipg_start;
    right = nt_SceneLineItemBlock._ipg_end;

    // { data = SceneLineItemBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneLineItemBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTextItemBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 6 ]
    if (!(a_blockType == 6)) break _ipg_alt;

    // SceneTextItemBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTextItemBlock = SceneTextItemBlock(input, begin + left, begin + right);
    if (nt_SceneTextItemBlock === null) break _ipg_alt;
    if (nt_SceneTextItemBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTextItemBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTextItemBlock._ipg_end);
    }
    nt_SceneTextItemBlock._ipg_end += left;
    nt_SceneTextItemBlock._ipg_start += left;
    left = nt_SceneTextItemBlock._ipg_start;
    right = nt_SceneTextItemBlock._ipg_end;

    // { data = SceneTextItemBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTextItemBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RootTextBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 7 ]
    if (!(a_blockType == 7)) break _ipg_alt;

    // RootTextBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RootTextBlock = RootTextBlock(input, begin + left, begin + right);
    if (nt_RootTextBlock === null) break _ipg_alt;
    if (nt_RootTextBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RootTextBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RootTextBlock._ipg_end);
    }
    nt_RootTextBlock._ipg_end += left;
    nt_RootTextBlock._ipg_start += left;
    left = nt_RootTextBlock._ipg_start;
    right = nt_RootTextBlock._ipg_end;

    // { data = RootTextBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_RootTextBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTombstoneItemBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 8 ]
    if (!(a_blockType == 8)) break _ipg_alt;

    // SceneTombstoneItemBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTombstoneItemBlock = SceneTombstoneItemBlock(input, begin + left, begin + right);
    if (nt_SceneTombstoneItemBlock === null) break _ipg_alt;
    if (nt_SceneTombstoneItemBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTombstoneItemBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTombstoneItemBlock._ipg_end);
    }
    nt_SceneTombstoneItemBlock._ipg_end += left;
    nt_SceneTombstoneItemBlock._ipg_start += left;
    left = nt_SceneTombstoneItemBlock._ipg_start;
    right = nt_SceneTombstoneItemBlock._ipg_end;

    // { data = SceneTombstoneItemBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTombstoneItemBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_AuthorIdsBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 9 ]
    if (!(a_blockType == 9)) break _ipg_alt;

    // AuthorIdsBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AuthorIdsBlock = AuthorIdsBlock(input, begin + left, begin + right);
    if (nt_AuthorIdsBlock === null) break _ipg_alt;
    if (nt_AuthorIdsBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AuthorIdsBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AuthorIdsBlock._ipg_end);
    }
    nt_AuthorIdsBlock._ipg_end += left;
    nt_AuthorIdsBlock._ipg_start += left;
    left = nt_AuthorIdsBlock._ipg_start;
    right = nt_AuthorIdsBlock._ipg_end;

    // { data = AuthorIdsBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_AuthorIdsBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_PageInfoBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 10 ]
    if (!(a_blockType == 10)) break _ipg_alt;

    // PageInfoBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PageInfoBlock = PageInfoBlock(input, begin + left, begin + right);
    if (nt_PageInfoBlock === null) break _ipg_alt;
    if (nt_PageInfoBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PageInfoBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PageInfoBlock._ipg_end);
    }
    nt_PageInfoBlock._ipg_end += left;
    nt_PageInfoBlock._ipg_start += left;
    left = nt_PageInfoBlock._ipg_start;
    right = nt_PageInfoBlock._ipg_end;

    // { data = PageInfoBlock.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_PageInfoBlock);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneInfo;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 13 ]
    if (!(a_blockType == 13)) break _ipg_alt;

    // SceneInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneInfo = SceneInfo(input, begin + left, begin + right);
    if (nt_SceneInfo === null) break _ipg_alt;
    if (nt_SceneInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneInfo._ipg_end);
    }
    nt_SceneInfo._ipg_end += left;
    nt_SceneInfo._ipg_start += left;
    left = nt_SceneInfo._ipg_start;
    right = nt_SceneInfo._ipg_end;

    // { data = SceneInfo.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneInfo);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_UnknownBlock;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // UnknownBlock[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_UnknownBlock = UnknownBlock(input, begin + left, begin + right);
    if (nt_UnknownBlock === null) break _ipg_alt;
    if (nt_UnknownBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_UnknownBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_UnknownBlock._ipg_end);
    }
    nt_UnknownBlock._ipg_end += left;
    nt_UnknownBlock._ipg_start += left;
    left = nt_UnknownBlock._ipg_start;
    right = nt_UnknownBlock._ipg_end;

    // { data = UnknownBlock.data }
    self.data = nt_UnknownBlock.data;

    return self;
  }

  
  return null;
}

function AuthorIdsBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt;
    let nt_AuthorId;
    let seq_AuthorId; let seq_AuthorId_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VarUInt[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // for i = 0 to VarUInt.value do AuthorId[AuthorId.END, EOI]
    nt_AuthorId = { _ipg_end: right, _ipg_start: left };
    seq_AuthorId_start = 0;
    loopEnd = nt_VarUInt.value;
    seq_AuthorId = new Array(loopEnd - seq_AuthorId_start);
    for (self.i = seq_AuthorId_start; self.i < loopEnd; self.i++) {
      const left = nt_AuthorId._ipg_end;
      const right = EOI;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = AuthorId(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_AuthorId._ipg_end = tmp._ipg_end;
      nt_AuthorId._ipg_start = tmp._ipg_start;
      seq_AuthorId[self.i - seq_AuthorId_start] = tmp;
    }
    delete self.i;
    left = nt_AuthorId._ipg_start;
    right = nt_AuthorId._ipg_end;

    // { authorIds = AuthorId.these }
    self.authorIds = seq_AuthorId.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function AuthorId(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_VarUInt;
    let nt_U16;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(0)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 0);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // ?[ check(VarUInt.value == 16, "Expect UUID to have length 16") ]
    if (!check(nt_VarUInt.value == 16, "Expect UUID to have length 16")) break _ipg_alt;

    // { uuid = *[VarUInt.END, VarUInt.END + VarUInt.value] }
    left = nt_VarUInt._ipg_end;
    right = nt_VarUInt._ipg_end + nt_VarUInt.value;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.uuid = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // U16[VarUInt.END + VarUInt.value, SubBlock.END + SubBlock.length]
    left = nt_VarUInt._ipg_end + nt_VarUInt.value;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
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

    // { authorId = U16.value }
    self.authorId = nt_U16.value;

    // ExpectEmpty[U16.END, SubBlock.END + SubBlock.length]
    left = nt_U16._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function MigrationInfoBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    let nt_TaggedBool;
    let nt_OptionalTaggedBool;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { migrationId = TaggedId.value }
    self.migrationId = nt_TaggedId.value;

    // TaggedBool(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool = TaggedBool(input, begin + left, begin + right, 2);
    if (nt_TaggedBool === null) break _ipg_alt;
    if (nt_TaggedBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool._ipg_end);
    }
    nt_TaggedBool._ipg_end += left;
    nt_TaggedBool._ipg_start += left;
    left = nt_TaggedBool._ipg_start;
    right = nt_TaggedBool._ipg_end;

    // { isDevice = TaggedBool.value }
    self.isDevice = nt_TaggedBool.value;

    // OptionalTaggedBool(3)[TaggedBool.END, EOI]
    left = nt_TaggedBool._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalTaggedBool = OptionalTaggedBool(input, begin + left, begin + right, 3);
    if (nt_OptionalTaggedBool === null) break _ipg_alt;
    if (nt_OptionalTaggedBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalTaggedBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalTaggedBool._ipg_end);
    }
    nt_OptionalTaggedBool._ipg_end += left;
    nt_OptionalTaggedBool._ipg_start += left;
    left = nt_OptionalTaggedBool._ipg_start;
    right = nt_OptionalTaggedBool._ipg_end;

    // { unknown = OptionalTaggedBool.value }
    self.unknown = nt_OptionalTaggedBool.value;

    return self;
  }

  
  return null;
}

function TreeNodeBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    let nt_LWWString;
    let nt_LWWBool;
    let nt_OptionalLWWID;
    let nt_OptionalLWWU8;
    let nt_OptionalLWWFloat32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { nodeId = TaggedId.value }
    self.nodeId = nt_TaggedId.value;

    // LWWString(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWString = LWWString(input, begin + left, begin + right, 2);
    if (nt_LWWString === null) break _ipg_alt;
    if (nt_LWWString._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWString._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWString._ipg_end);
    }
    nt_LWWString._ipg_end += left;
    nt_LWWString._ipg_start += left;
    left = nt_LWWString._ipg_start;
    right = nt_LWWString._ipg_end;

    // { label = LWWString.value }
    self.label = nt_LWWString.value;

    // LWWBool(3)[LWWString.END, EOI]
    left = nt_LWWString._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWBool = LWWBool(input, begin + left, begin + right, 3);
    if (nt_LWWBool === null) break _ipg_alt;
    if (nt_LWWBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWBool._ipg_end);
    }
    nt_LWWBool._ipg_end += left;
    nt_LWWBool._ipg_start += left;
    left = nt_LWWBool._ipg_start;
    right = nt_LWWBool._ipg_end;

    // { visible = LWWBool.value }
    self.visible = nt_LWWBool.value;

    // OptionalLWWID(7)[LWWBool.END, EOI]
    left = nt_LWWBool._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWID = OptionalLWWID(input, begin + left, begin + right, 7);
    if (nt_OptionalLWWID === null) break _ipg_alt;
    if (nt_OptionalLWWID._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWID._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWID._ipg_end);
    }
    nt_OptionalLWWID._ipg_end += left;
    nt_OptionalLWWID._ipg_start += left;
    left = nt_OptionalLWWID._ipg_start;
    right = nt_OptionalLWWID._ipg_end;

    // { anchorId = OptionalLWWID.value }
    self.anchorId = nt_OptionalLWWID.value;

    // OptionalLWWU8(8)[OptionalLWWID.END, EOI]
    left = nt_OptionalLWWID._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWU8 = OptionalLWWU8(input, begin + left, begin + right, 8);
    if (nt_OptionalLWWU8 === null) break _ipg_alt;
    if (nt_OptionalLWWU8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWU8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWU8._ipg_end);
    }
    nt_OptionalLWWU8._ipg_end += left;
    nt_OptionalLWWU8._ipg_start += left;
    left = nt_OptionalLWWU8._ipg_start;
    right = nt_OptionalLWWU8._ipg_end;

    // { anchorType = OptionalLWWU8.value }
    self.anchorType = nt_OptionalLWWU8.value;

    // OptionalLWWFloat32(9)[OptionalLWWU8.END, EOI]
    left = nt_OptionalLWWU8._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWFloat32 = OptionalLWWFloat32(input, begin + left, begin + right, 9);
    if (nt_OptionalLWWFloat32 === null) break _ipg_alt;
    if (nt_OptionalLWWFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWFloat32._ipg_end);
    }
    nt_OptionalLWWFloat32._ipg_end += left;
    nt_OptionalLWWFloat32._ipg_start += left;
    left = nt_OptionalLWWFloat32._ipg_start;
    right = nt_OptionalLWWFloat32._ipg_end;

    // { anchorThreshold = OptionalLWWFloat32.value }
    self.anchorThreshold = nt_OptionalLWWFloat32.value;

    // OptionalLWWFloat32(10)[OptionalLWWFloat32.END, EOI]
    left = nt_OptionalLWWFloat32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWFloat32 = OptionalLWWFloat32(input, begin + left, begin + right, 10);
    if (nt_OptionalLWWFloat32 === null) break _ipg_alt;
    if (nt_OptionalLWWFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWFloat32._ipg_end);
    }
    nt_OptionalLWWFloat32._ipg_end += left;
    nt_OptionalLWWFloat32._ipg_start += left;
    left = nt_OptionalLWWFloat32._ipg_start;
    right = nt_OptionalLWWFloat32._ipg_end;

    // { anchorOriginX = OptionalLWWFloat32.value }
    self.anchorOriginX = nt_OptionalLWWFloat32.value;

    return self;
  }

  
  return null;
}

function PageInfoBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedU32;
    let nt_OptionalU32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedU32(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 1);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { loadsCount = TaggedU32.value }
    self.loadsCount = nt_TaggedU32.value;

    // TaggedU32(2)[TaggedU32.END, EOI]
    left = nt_TaggedU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 2);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { mergesCount = TaggedU32.value }
    self.mergesCount = nt_TaggedU32.value;

    // TaggedU32(3)[TaggedU32.END, EOI]
    left = nt_TaggedU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 3);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { textCharsCount = TaggedU32.value }
    self.textCharsCount = nt_TaggedU32.value;

    // TaggedU32(4)[TaggedU32.END, EOI]
    left = nt_TaggedU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 4);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { textLinesCount = TaggedU32.value }
    self.textLinesCount = nt_TaggedU32.value;

    // OptionalU32(5)[TaggedU32.END, EOI]
    left = nt_TaggedU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32 = OptionalU32(input, begin + left, begin + right, 5);
    if (nt_OptionalU32 === null) break _ipg_alt;
    if (nt_OptionalU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32._ipg_end);
    }
    nt_OptionalU32._ipg_end += left;
    nt_OptionalU32._ipg_start += left;
    left = nt_OptionalU32._ipg_start;
    right = nt_OptionalU32._ipg_end;

    // { typeFolioUseCount = OptionalU32.value }
    self.typeFolioUseCount = nt_OptionalU32.value;

    return self;
  }

  
  return null;
}

function SceneTreeBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    let nt_TaggedBool;
    let nt_SubBlock;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { treeId = TaggedId.value }
    self.treeId = nt_TaggedId.value;

    // TaggedId(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { nodeId = TaggedId.value }
    self.nodeId = nt_TaggedId.value;

    // TaggedBool(3)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool = TaggedBool(input, begin + left, begin + right, 3);
    if (nt_TaggedBool === null) break _ipg_alt;
    if (nt_TaggedBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool._ipg_end);
    }
    nt_TaggedBool._ipg_end += left;
    nt_TaggedBool._ipg_start += left;
    left = nt_TaggedBool._ipg_start;
    right = nt_TaggedBool._ipg_end;

    // { isUpdate = TaggedBool.value }
    self.isUpdate = nt_TaggedBool.value;

    // SubBlock(4)[TaggedBool.END, EOI]
    left = nt_TaggedBool._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 4);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { parentId = TaggedId.value }
    self.parentId = nt_TaggedId.value;

    // ExpectEmpty[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function SceneInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWID;
    let nt_OptionalLWWBool;
    let nt_OptionalIntPair;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWID(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWID = LWWID(input, begin + left, begin + right, 1);
    if (nt_LWWID === null) break _ipg_alt;
    if (nt_LWWID._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWID._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWID._ipg_end);
    }
    nt_LWWID._ipg_end += left;
    nt_LWWID._ipg_start += left;
    left = nt_LWWID._ipg_start;
    right = nt_LWWID._ipg_end;

    // { currentLayer = LWWID.value }
    self.currentLayer = nt_LWWID.value;

    // OptionalLWWBool(2)[LWWID.END, EOI]
    left = nt_LWWID._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWBool = OptionalLWWBool(input, begin + left, begin + right, 2);
    if (nt_OptionalLWWBool === null) break _ipg_alt;
    if (nt_OptionalLWWBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWBool._ipg_end);
    }
    nt_OptionalLWWBool._ipg_end += left;
    nt_OptionalLWWBool._ipg_start += left;
    left = nt_OptionalLWWBool._ipg_start;
    right = nt_OptionalLWWBool._ipg_end;

    // { backgroundVisible = OptionalLWWBool.value }
    self.backgroundVisible = nt_OptionalLWWBool.value;

    // OptionalLWWBool(3)[OptionalLWWBool.END, EOI]
    left = nt_OptionalLWWBool._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWBool = OptionalLWWBool(input, begin + left, begin + right, 3);
    if (nt_OptionalLWWBool === null) break _ipg_alt;
    if (nt_OptionalLWWBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWBool._ipg_end);
    }
    nt_OptionalLWWBool._ipg_end += left;
    nt_OptionalLWWBool._ipg_start += left;
    left = nt_OptionalLWWBool._ipg_start;
    right = nt_OptionalLWWBool._ipg_end;

    // { rootDocumentVisible = OptionalLWWBool.value }
    self.rootDocumentVisible = nt_OptionalLWWBool.value;

    // OptionalIntPair(4)[OptionalLWWBool.END, EOI]
    left = nt_OptionalLWWBool._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalIntPair = OptionalIntPair(input, begin + left, begin + right, 4);
    if (nt_OptionalIntPair === null) break _ipg_alt;
    if (nt_OptionalIntPair._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalIntPair._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalIntPair._ipg_end);
    }
    nt_OptionalIntPair._ipg_end += left;
    nt_OptionalIntPair._ipg_start += left;
    left = nt_OptionalIntPair._ipg_start;
    right = nt_OptionalIntPair._ipg_end;

    // { paperSize = OptionalIntPair.value }
    self.paperSize = nt_OptionalIntPair.value;

    return self;
  }

  
  return null;
}

function SceneItemInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    let nt_TaggedU32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { parentId = TaggedId.value }
    self.parentId = nt_TaggedId.value;

    // TaggedId(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { itemId = TaggedId.value }
    self.itemId = nt_TaggedId.value;

    // TaggedId(3)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 3);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { leftId = TaggedId.value }
    self.leftId = nt_TaggedId.value;

    // TaggedId(4)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 4);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { rightId = TaggedId.value }
    self.rightId = nt_TaggedId.value;

    // TaggedU32(5)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 5);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { deletedLength = TaggedU32.value }
    self.deletedLength = nt_TaggedU32.value;

    return self;
  }

  
  return null;
}

function SceneTombstoneItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo === null) break _ipg_alt;
    if (nt_SceneItemInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo._ipg_end);
    }
    nt_SceneItemInfo._ipg_end += left;
    nt_SceneItemInfo._ipg_start += left;
    left = nt_SceneItemInfo._ipg_start;
    right = nt_SceneItemInfo._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo.this) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo));

    return self;
  }

  
  return null;
}

function SceneGlyphItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo;
    let nt_SceneGlyphItemBlockValue;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo === null) break _ipg_alt;
    if (nt_SceneItemInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo._ipg_end);
    }
    nt_SceneItemInfo._ipg_end += left;
    nt_SceneItemInfo._ipg_start += left;
    left = nt_SceneItemInfo._ipg_start;
    right = nt_SceneItemInfo._ipg_end;

    // SceneGlyphItemBlockValue[SceneItemInfo.END, EOI]
    left = nt_SceneItemInfo._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGlyphItemBlockValue = SceneGlyphItemBlockValue(input, begin + left, begin + right);
    if (nt_SceneGlyphItemBlockValue === null) break _ipg_alt;
    if (nt_SceneGlyphItemBlockValue._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGlyphItemBlockValue._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGlyphItemBlockValue._ipg_end);
    }
    nt_SceneGlyphItemBlockValue._ipg_end += left;
    nt_SceneGlyphItemBlockValue._ipg_start += left;
    left = nt_SceneGlyphItemBlockValue._ipg_start;
    right = nt_SceneGlyphItemBlockValue._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo.this, SceneGlyphItemBlockValue.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo), nt_SceneGlyphItemBlockValue.value);

    return self;
  }

  
  return null;
}

function SceneGlyphItemBlockValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_OptionalU32;
    let nt_TaggedU32;
    let nt_String;
    let nt_VarUInt;
    let nt_Rectangle;
    let nt_ExpectEmpty;
    let seq_Rectangle; let seq_Rectangle_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(6)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { outerEnd = SubBlock.END + SubBlock.length }
    self.outerEnd = nt_SubBlock._ipg_end + nt_SubBlock.length;

    // "\x01"[SubBlock.END, SubBlock.END + 1]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // OptionalU32(2)[SubBlock.END + 1, EOI]
    left = nt_SubBlock._ipg_end + 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32 = OptionalU32(input, begin + left, begin + right, 2);
    if (nt_OptionalU32 === null) break _ipg_alt;
    if (nt_OptionalU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32._ipg_end);
    }
    nt_OptionalU32._ipg_end += left;
    nt_OptionalU32._ipg_start += left;
    left = nt_OptionalU32._ipg_start;
    right = nt_OptionalU32._ipg_end;

    // { optStart = OptionalU32.value }
    self.optStart = nt_OptionalU32.value;

    // OptionalU32(3)[OptionalU32.END, EOI]
    left = nt_OptionalU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32 = OptionalU32(input, begin + left, begin + right, 3);
    if (nt_OptionalU32 === null) break _ipg_alt;
    if (nt_OptionalU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32._ipg_end);
    }
    nt_OptionalU32._ipg_end += left;
    nt_OptionalU32._ipg_start += left;
    left = nt_OptionalU32._ipg_start;
    right = nt_OptionalU32._ipg_end;

    // { optLength = OptionalU32.value }
    self.optLength = nt_OptionalU32.value;

    // TaggedU32(4)[OptionalU32.END, EOI]
    left = nt_OptionalU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 4);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { colorId = TaggedU32.value }
    self.colorId = nt_TaggedU32.value;

    // String(5)[TaggedU32.END, EOI]
    left = nt_TaggedU32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String = String(input, begin + left, begin + right, 5);
    if (nt_String === null) break _ipg_alt;
    if (nt_String._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String._ipg_end);
    }
    nt_String._ipg_end += left;
    nt_String._ipg_start += left;
    left = nt_String._ipg_start;
    right = nt_String._ipg_end;

    // { text = String.value }
    self.text = nt_String.value;

    // { start = optStart == getNull() ? 0 : optStart }
    self.start = self.optStart == getNull() ? 0 : self.optStart;

    // { length = optLength == getNull() ? getLength(text) : optLength }
    self.length = self.optLength == getNull() ? getLength(self.text) : self.optLength;

    // SubBlock(6)[String.END, EOI]
    left = nt_String._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END + 1, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end + 1;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // for i = 0 to VarUInt.value do Rectangle[VarUInt.END + 32 * i, SubBlock.END + SubBlock.length]
    nt_Rectangle = { _ipg_end: right, _ipg_start: left };
    seq_Rectangle_start = 0;
    loopEnd = nt_VarUInt.value;
    seq_Rectangle = new Array(loopEnd - seq_Rectangle_start);
    for (self.i = seq_Rectangle_start; self.i < loopEnd; self.i++) {
      const left = nt_VarUInt._ipg_end + 32 * self.i;
      const right = nt_SubBlock._ipg_end + nt_SubBlock.length;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Rectangle(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Rectangle._ipg_end = tmp._ipg_end;
      nt_Rectangle._ipg_start = tmp._ipg_start;
      seq_Rectangle[self.i - seq_Rectangle_start] = tmp;
    }
    delete self.i;
    left = nt_Rectangle._ipg_start;
    right = nt_Rectangle._ipg_end;

    // { rectangles = Rectangle.these }
    self.rectangles = seq_Rectangle.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty[Rectangle.END, SubBlock.END + SubBlock.length]
    left = nt_Rectangle._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // ExpectEmpty[ExpectEmpty.END, outerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // { value = makeSceneGlyItemBlockValue(start, length, colorId, text, rectangles) }
    self.value = makeSceneGlyItemBlockValue(self.start, self.length, self.colorId, self.text, self.rectangles);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function Rectangle(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Float64[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { x = Float64.value }
    self.x = nt_Float64.value;

    // Float64[Float64.END, EOI]
    left = nt_Float64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { y = Float64.value }
    self.y = nt_Float64.value;

    // Float64[Float64.END, EOI]
    left = nt_Float64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { width = Float64.value }
    self.width = nt_Float64.value;

    // Float64[Float64.END, EOI]
    left = nt_Float64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { height = Float64.value }
    self.height = nt_Float64.value;

    return self;
  }

  
  return null;
}

function SceneGroupItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo;
    let nt_SceneGroupItemBlockValue;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo === null) break _ipg_alt;
    if (nt_SceneItemInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo._ipg_end);
    }
    nt_SceneItemInfo._ipg_end += left;
    nt_SceneItemInfo._ipg_start += left;
    left = nt_SceneItemInfo._ipg_start;
    right = nt_SceneItemInfo._ipg_end;

    // SceneGroupItemBlockValue[SceneItemInfo.END, EOI]
    left = nt_SceneItemInfo._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGroupItemBlockValue = SceneGroupItemBlockValue(input, begin + left, begin + right);
    if (nt_SceneGroupItemBlockValue === null) break _ipg_alt;
    if (nt_SceneGroupItemBlockValue._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGroupItemBlockValue._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGroupItemBlockValue._ipg_end);
    }
    nt_SceneGroupItemBlockValue._ipg_end += left;
    nt_SceneGroupItemBlockValue._ipg_start += left;
    left = nt_SceneGroupItemBlockValue._ipg_start;
    right = nt_SceneGroupItemBlockValue._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo.this, SceneGroupItemBlockValue.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo), nt_SceneGroupItemBlockValue.value);

    return self;
  }

  
  return null;
}

function SceneGroupItemBlockValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(6)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // "\x02"[SubBlock.END, SubBlock.END + 1]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x02")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // TaggedId(2)[SubBlock.END + 1, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end + 1;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { value = TaggedId.value }
    self.value = nt_TaggedId.value;

    // ExpectEmpty[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function SceneLineItemBlock(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo;
    let nt_SceneLineItemBlockValue;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo === null) break _ipg_alt;
    if (nt_SceneItemInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo._ipg_end);
    }
    nt_SceneItemInfo._ipg_end += left;
    nt_SceneItemInfo._ipg_start += left;
    left = nt_SceneItemInfo._ipg_start;
    right = nt_SceneItemInfo._ipg_end;

    // SceneLineItemBlockValue(version)[SceneItemInfo.END, EOI]
    left = nt_SceneItemInfo._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneLineItemBlockValue = SceneLineItemBlockValue(input, begin + left, begin + right, a_version);
    if (nt_SceneLineItemBlockValue === null) break _ipg_alt;
    if (nt_SceneLineItemBlockValue._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneLineItemBlockValue._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneLineItemBlockValue._ipg_end);
    }
    nt_SceneLineItemBlockValue._ipg_end += left;
    nt_SceneLineItemBlockValue._ipg_start += left;
    left = nt_SceneLineItemBlockValue._ipg_start;
    right = nt_SceneLineItemBlockValue._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo.this, SceneLineItemBlockValue.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo), nt_SceneLineItemBlockValue.value);

    return self;
  }

  
  return null;
}

function SceneLineItemBlockValue(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedU32;
    let nt_TaggedFloat64;
    let nt_TaggedFloat32;
    let nt_Point;
    let nt_ExpectEmpty;
    let nt_TaggedId;
    let nt_OptionalTaggedId;
    let seq_Point; let seq_Point_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(6)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { outerEnd = SubBlock.END + SubBlock.length }
    self.outerEnd = nt_SubBlock._ipg_end + nt_SubBlock.length;

    // "\x03"[SubBlock.END, SubBlock.END + 1]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x03")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // TaggedU32(1)[SubBlock.END + 1, outerEnd]
    left = nt_SubBlock._ipg_end + 1;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 1);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { toolId = TaggedU32.value }
    self.toolId = nt_TaggedU32.value;

    // TaggedU32(2)[TaggedU32.END, outerEnd]
    left = nt_TaggedU32._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 2);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { colorId = TaggedU32.value }
    self.colorId = nt_TaggedU32.value;

    // TaggedFloat64(3)[TaggedU32.END, outerEnd]
    left = nt_TaggedU32._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat64 = TaggedFloat64(input, begin + left, begin + right, 3);
    if (nt_TaggedFloat64 === null) break _ipg_alt;
    if (nt_TaggedFloat64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat64._ipg_end);
    }
    nt_TaggedFloat64._ipg_end += left;
    nt_TaggedFloat64._ipg_start += left;
    left = nt_TaggedFloat64._ipg_start;
    right = nt_TaggedFloat64._ipg_end;

    // { thicknessScale = TaggedFloat64.value }
    self.thicknessScale = nt_TaggedFloat64.value;

    // TaggedFloat32(4)[TaggedFloat64.END, outerEnd]
    left = nt_TaggedFloat64._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32 = TaggedFloat32(input, begin + left, begin + right, 4);
    if (nt_TaggedFloat32 === null) break _ipg_alt;
    if (nt_TaggedFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32._ipg_end);
    }
    nt_TaggedFloat32._ipg_end += left;
    nt_TaggedFloat32._ipg_start += left;
    left = nt_TaggedFloat32._ipg_start;
    right = nt_TaggedFloat32._ipg_end;

    // { startLength = TaggedFloat32.value }
    self.startLength = nt_TaggedFloat32.value;

    // SubBlock(5)[TaggedFloat32.END, outerEnd]
    left = nt_TaggedFloat32._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 5);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { pointSize = pointSerializedSize(version) }
    self.pointSize = pointSerializedSize(a_version);

    // ?[ check(SubBlock.length % pointSize == 0, "Point size does not divide subblock size") ]
    if (!check(nt_SubBlock.length % self.pointSize == 0, "Point size does not divide subblock size")) break _ipg_alt;

    // for i = 0 to SubBlock.length / pointSize do Point(version)[SubBlock.END + pointSize * i, SubBlock.END + pointSize * (i + 1)]
    nt_Point = { _ipg_end: right, _ipg_start: left };
    seq_Point_start = 0;
    loopEnd = nt_SubBlock.length / self.pointSize;
    seq_Point = new Array(loopEnd - seq_Point_start);
    for (self.i = seq_Point_start; self.i < loopEnd; self.i++) {
      const left = nt_SubBlock._ipg_end + self.pointSize * self.i;
      const right = nt_SubBlock._ipg_end + self.pointSize * (self.i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Point(input, begin + left, begin + right, a_version);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Point._ipg_end = tmp._ipg_end;
      nt_Point._ipg_start = tmp._ipg_start;
      seq_Point[self.i - seq_Point_start] = tmp;
    }
    delete self.i;
    left = nt_Point._ipg_start;
    right = nt_Point._ipg_end;

    // { points = Point.these }
    self.points = seq_Point.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty[Point.END, SubBlock.END + SubBlock.length]
    left = nt_Point._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // TaggedId(6)[ExpectEmpty.END, outerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 6);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { timestamp = TaggedId.value }
    self.timestamp = nt_TaggedId.value;

    // OptionalTaggedId(7)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalTaggedId = OptionalTaggedId(input, begin + left, begin + right, 7);
    if (nt_OptionalTaggedId === null) break _ipg_alt;
    if (nt_OptionalTaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalTaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalTaggedId._ipg_end);
    }
    nt_OptionalTaggedId._ipg_end += left;
    nt_OptionalTaggedId._ipg_start += left;
    left = nt_OptionalTaggedId._ipg_start;
    right = nt_OptionalTaggedId._ipg_end;

    // { moveId = OptionalTaggedId.value }
    self.moveId = nt_OptionalTaggedId.value;

    // ExpectEmpty[OptionalTaggedId.END, outerEnd]
    left = nt_OptionalTaggedId._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // { value = makeSceneLineItemBlockValue(toolId, colorId, thicknessScale, startLength, points, moveId) }
    self.value = makeSceneLineItemBlockValue(self.toolId, self.colorId, self.thicknessScale, self.startLength, self.points, self.moveId);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function Point(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float32;
    let nt_PointVersions;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Float32[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { x = Float32.value }
    self.x = nt_Float32.value;

    // Float32[Float32.END, EOI]
    left = nt_Float32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { y = Float32.value }
    self.y = nt_Float32.value;

    // PointVersions(version)[Float32.END, EOI]
    left = nt_Float32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PointVersions = PointVersions(input, begin + left, begin + right, a_version);
    if (nt_PointVersions === null) break _ipg_alt;
    if (nt_PointVersions._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PointVersions._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PointVersions._ipg_end);
    }
    nt_PointVersions._ipg_end += left;
    nt_PointVersions._ipg_start += left;
    left = nt_PointVersions._ipg_start;
    right = nt_PointVersions._ipg_end;

    // { speed = PointVersions.speed }
    self.speed = nt_PointVersions.speed;

    // { width = PointVersions.width }
    self.width = nt_PointVersions.width;

    // { direction = PointVersions.direction }
    self.direction = nt_PointVersions.direction;

    // { pressure = PointVersions.pressure }
    self.pressure = nt_PointVersions.pressure;

    return self;
  }

  
  return null;
}

function PointVersions(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ version == 1 ]
    if (!(a_version == 1)) break _ipg_alt;

    // Float32[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { speed = 4 * Float32.value }
    self.speed = 4 * nt_Float32.value;

    // Float32[Float32.END, EOI]
    left = nt_Float32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { direction = 255 * Float32.value / getTwoPi() }
    self.direction = 255 * nt_Float32.value / getTwoPi();

    // Float32[Float32.END, EOI]
    left = nt_Float32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { width = int(round(4 * Float32.value)) }
    self.width = int(round(4 * nt_Float32.value));

    // Float32[Float32.END, EOI]
    left = nt_Float32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { pressure = 255 * Float32.value }
    self.pressure = 255 * nt_Float32.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ version == 2 ]
    if (!(a_version == 2)) break _ipg_alt;

    // U16[0, EOI]
    left = 0;
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

    // { speed = U16.value }
    self.speed = nt_U16.value;

    // U16[U16.END, EOI]
    left = nt_U16._ipg_end;
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

    // { direction = .[U16.END] }
    left = nt_U16._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.direction = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { pressure = .[U16.END + 1] }
    left = nt_U16._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.pressure = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

function SceneTextItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo === null) break _ipg_alt;
    if (nt_SceneItemInfo._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo._ipg_end);
    }
    nt_SceneItemInfo._ipg_end += left;
    nt_SceneItemInfo._ipg_start += left;
    left = nt_SceneItemInfo._ipg_start;
    right = nt_SceneItemInfo._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo.this) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo));

    return self;
  }

  
  return null;
}

function RootTextBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    let nt_SubBlock;
    let nt_VarUInt;
    let nt_TextItem;
    let nt_ExpectEmpty;
    let nt_TextFormat;
    let nt_Float64;
    let nt_TaggedFloat32;
    let seq_TextItem; let seq_TextItem_start = 0;
    let seq_TextFormat; let seq_TextFormat_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { blockId = TaggedId.value }
    self.blockId = nt_TaggedId.value;

    // ?[ checkRootTextBlockId(blockId) ]
    if (!checkRootTextBlockId(self.blockId)) break _ipg_alt;

    // SubBlock(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { outerEnd = SubBlock.END + SubBlock.length }
    self.outerEnd = nt_SubBlock._ipg_end + nt_SubBlock.length;

    // SubBlock(1)[SubBlock.END, outerEnd]
    left = nt_SubBlock._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { innerEnd = SubBlock.END + SubBlock.length }
    self.innerEnd = nt_SubBlock._ipg_end + nt_SubBlock.length;

    // SubBlock(1)[SubBlock.END, innerEnd]
    left = nt_SubBlock._ipg_end;
    right = self.innerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END, EOI]
    left = nt_SubBlock._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // for i = 0 to VarUInt.value do TextItem[TextItem.END, SubBlock.END + SubBlock.length]
    nt_TextItem = { _ipg_end: right, _ipg_start: left };
    seq_TextItem_start = 0;
    loopEnd = nt_VarUInt.value;
    seq_TextItem = new Array(loopEnd - seq_TextItem_start);
    for (self.i = seq_TextItem_start; self.i < loopEnd; self.i++) {
      const left = nt_TextItem._ipg_end;
      const right = nt_SubBlock._ipg_end + nt_SubBlock.length;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = TextItem(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_TextItem._ipg_end = tmp._ipg_end;
      nt_TextItem._ipg_start = tmp._ipg_start;
      seq_TextItem[self.i - seq_TextItem_start] = tmp;
    }
    delete self.i;
    left = nt_TextItem._ipg_start;
    right = nt_TextItem._ipg_end;

    // { textItems = TextItem.these }
    self.textItems = seq_TextItem.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty[TextItem.END, SubBlock.END + SubBlock.length]
    left = nt_TextItem._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // ExpectEmpty[ExpectEmpty.END, innerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.innerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // SubBlock(2)[ExpectEmpty.END, outerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // { innerEnd = SubBlock.END + SubBlock.length }
    self.innerEnd = nt_SubBlock._ipg_end + nt_SubBlock.length;

    // SubBlock(1)[SubBlock.END, innerEnd]
    left = nt_SubBlock._ipg_end;
    right = self.innerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END, EOI]
    left = nt_SubBlock._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // for i = 0 to VarUInt.value do TextFormat[TextFormat.END, SubBlock.END + SubBlock.length]
    nt_TextFormat = { _ipg_end: right, _ipg_start: left };
    seq_TextFormat_start = 0;
    loopEnd = nt_VarUInt.value;
    seq_TextFormat = new Array(loopEnd - seq_TextFormat_start);
    for (self.i = seq_TextFormat_start; self.i < loopEnd; self.i++) {
      const left = nt_TextFormat._ipg_end;
      const right = nt_SubBlock._ipg_end + nt_SubBlock.length;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = TextFormat(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_TextFormat._ipg_end = tmp._ipg_end;
      nt_TextFormat._ipg_start = tmp._ipg_start;
      seq_TextFormat[self.i - seq_TextFormat_start] = tmp;
    }
    delete self.i;
    left = nt_TextFormat._ipg_start;
    right = nt_TextFormat._ipg_end;

    // { textFormats = TextFormat.these }
    self.textFormats = seq_TextFormat.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty[TextFormat.END, SubBlock.END + SubBlock.length]
    left = nt_TextFormat._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // ExpectEmpty[ExpectEmpty.END, innerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.innerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // ExpectEmpty[ExpectEmpty.END, outerEnd]
    left = nt_ExpectEmpty._ipg_end;
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // SubBlock(3)[ExpectEmpty.END, EOI]
    left = nt_ExpectEmpty._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 3);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // Float64[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { xPosition = Float64.value }
    self.xPosition = nt_Float64.value;

    // Float64[Float64.END, SubBlock.END + SubBlock.length]
    left = nt_Float64._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { yPosition = Float64.value }
    self.yPosition = nt_Float64.value;

    // ExpectEmpty[ExpectEmpty.END, SubBlock.END + SubBlock.length]
    left = nt_ExpectEmpty._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // TaggedFloat32(4)[ExpectEmpty.END, EOI]
    left = nt_ExpectEmpty._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32 = TaggedFloat32(input, begin + left, begin + right, 4);
    if (nt_TaggedFloat32 === null) break _ipg_alt;
    if (nt_TaggedFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32._ipg_end);
    }
    nt_TaggedFloat32._ipg_end += left;
    nt_TaggedFloat32._ipg_start += left;
    left = nt_TaggedFloat32._ipg_start;
    right = nt_TaggedFloat32._ipg_end;

    // { width = TaggedFloat32.value }
    self.width = nt_TaggedFloat32.value;

    return self;
  }

  
  return null;
}

function TextItem(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_TaggedU32;
    let nt_TextItemValue;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(0)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 0);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(2)[SubBlock.END, EOI]
    left = nt_SubBlock._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { itemId = TaggedId.value }
    self.itemId = nt_TaggedId.value;

    // TaggedId(3)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 3);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { leftId = TaggedId.value }
    self.leftId = nt_TaggedId.value;

    // TaggedId(4)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 4);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { rightId = TaggedId.value }
    self.rightId = nt_TaggedId.value;

    // TaggedU32(5)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, 5);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { deletedLength = TaggedU32.value }
    self.deletedLength = nt_TaggedU32.value;

    // TextItemValue[TaggedU32.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedU32._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TextItemValue = TextItemValue(input, begin + left, begin + right);
    if (nt_TextItemValue === null) break _ipg_alt;
    if (nt_TextItemValue._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TextItemValue._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TextItemValue._ipg_end);
    }
    nt_TextItemValue._ipg_end += left;
    nt_TextItemValue._ipg_start += left;
    left = nt_TextItemValue._ipg_start;
    right = nt_TextItemValue._ipg_end;

    // { value = TextItemValue.value }
    self.value = nt_TextItemValue.value;

    // ExpectEmpty[TextItemValue.END, SubBlock.END + SubBlock.length]
    left = nt_TextItemValue._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function TextItemValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_StringWithFormat;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // StringWithFormat(6)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_StringWithFormat = StringWithFormat(input, begin + left, begin + right, 6);
    if (nt_StringWithFormat === null) break _ipg_alt;
    if (nt_StringWithFormat._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_StringWithFormat._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_StringWithFormat._ipg_end);
    }
    nt_StringWithFormat._ipg_end += left;
    nt_StringWithFormat._ipg_start += left;
    left = nt_StringWithFormat._ipg_start;
    right = nt_StringWithFormat._ipg_end;

    // { value = processTextItemValue(StringWithFormat.value) }
    self.value = processTextItemValue(nt_StringWithFormat.value);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = "" }
    self.value = "";

    return self;
  }

  
  return null;
}

function StringWithFormat(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_VarUInt;
    let nt_Bool;
    let nt_Bytes;
    let nt_OptionalU32;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // Bool[VarUInt.END, SubBlock.END + SubBlock.length]
    left = nt_VarUInt._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool = Bool(input, begin + left, begin + right);
    if (nt_Bool === null) break _ipg_alt;
    if (nt_Bool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool._ipg_end);
    }
    nt_Bool._ipg_end += left;
    nt_Bool._ipg_start += left;
    left = nt_Bool._ipg_start;
    right = nt_Bool._ipg_end;

    // ?[ check(Bool.value, "StringWithFormat flag unset") ]
    if (!check(nt_Bool.value, "StringWithFormat flag unset")) break _ipg_alt;

    // ?[ check(Bool.END + VarUInt.value <= SubBlock.END + SubBlock.length, "StringWithFormat: Overfull block") ]
    if (!check(nt_Bool._ipg_end + nt_VarUInt.value <= nt_SubBlock._ipg_end + nt_SubBlock.length, "StringWithFormat: Overfull block")) break _ipg_alt;

    // Bytes[Bool.END, Bool.END + VarUInt.value]
    left = nt_Bool._ipg_end;
    right = nt_Bool._ipg_end + nt_VarUInt.value;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes = Bytes(input, begin + left, begin + right);
    if (nt_Bytes === null) break _ipg_alt;
    if (nt_Bytes._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes._ipg_end);
    }
    nt_Bytes._ipg_end += left;
    nt_Bytes._ipg_start += left;
    left = nt_Bytes._ipg_start;
    right = nt_Bytes._ipg_end;

    // OptionalU32(2)[Bytes.END, SubBlock.END + SubBlock.length]
    left = nt_Bytes._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32 = OptionalU32(input, begin + left, begin + right, 2);
    if (nt_OptionalU32 === null) break _ipg_alt;
    if (nt_OptionalU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32._ipg_end);
    }
    nt_OptionalU32._ipg_end += left;
    nt_OptionalU32._ipg_start += left;
    left = nt_OptionalU32._ipg_start;
    right = nt_OptionalU32._ipg_end;

    // { value = makeStringWithFormat(decodeAscii(Bytes.value), OptionalU32.value) }
    self.value = makeStringWithFormat(decodeAscii(nt_Bytes.value), nt_OptionalU32.value);

    // ExpectEmpty[OptionalU32.END, SubBlock.END + SubBlock.length]
    left = nt_OptionalU32._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function TextFormat(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_CrdtId;
    let nt_TaggedId;
    let nt_SubBlock;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // CrdtId[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CrdtId = CrdtId(input, begin + left, begin + right);
    if (nt_CrdtId === null) break _ipg_alt;
    if (nt_CrdtId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CrdtId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CrdtId._ipg_end);
    }
    nt_CrdtId._ipg_end += left;
    nt_CrdtId._ipg_start += left;
    left = nt_CrdtId._ipg_start;
    right = nt_CrdtId._ipg_end;

    // { charId = CrdtId.this }
    self.charId = (({_ipg_start,_ipg_end,...o}) => o)(nt_CrdtId);

    // TaggedId(1)[CrdtId.END, EOI]
    left = nt_CrdtId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { timestamp = TaggedId.value }
    self.timestamp = nt_TaggedId.value;

    // SubBlock(2)[TaggedId.END, EOI]
    left = nt_TaggedId._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // "\x11"[SubBlock.END, SubBlock.END + 1]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x11")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { formatCode = .[SubBlock.END + 1] }
    left = nt_SubBlock._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.formatCode = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ExpectEmpty[SubBlock.END + 2, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end + 2;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    // { value = lwwU8(timestamp, formatCode < 0 || formatCode > 7 ? 1 : formatCode) }
    self.value = lwwU8(self.timestamp, self.formatCode < 0 || self.formatCode > 7 ? 1 : self.formatCode);

    return self;
  }

  
  return null;
}

function UnknownBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { data = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.data = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

function OptionalLWWBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWBool;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWBool(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWBool = LWWBool(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWBool === null) break _ipg_alt;
    if (nt_LWWBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWBool._ipg_end);
    }
    nt_LWWBool._ipg_end += left;
    nt_LWWBool._ipg_start += left;
    left = nt_LWWBool._ipg_start;
    right = nt_LWWBool._ipg_end;

    // { value = LWWBool.value }
    self.value = nt_LWWBool.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function LWWBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_TaggedBool;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // TaggedBool(2)[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool = TaggedBool(input, begin + left, begin + right, 2);
    if (nt_TaggedBool === null) break _ipg_alt;
    if (nt_TaggedBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool._ipg_end);
    }
    nt_TaggedBool._ipg_end += left;
    nt_TaggedBool._ipg_start += left;
    left = nt_TaggedBool._ipg_start;
    right = nt_TaggedBool._ipg_end;

    // { value = lwwBool(TaggedId.value, TaggedBool.value) }
    self.value = lwwBool(nt_TaggedId.value, nt_TaggedBool.value);

    // ExpectEmpty[TaggedBool.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedBool._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function OptionalLWWU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWU8;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWU8(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWU8 = LWWU8(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWU8 === null) break _ipg_alt;
    if (nt_LWWU8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWU8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWU8._ipg_end);
    }
    nt_LWWU8._ipg_end += left;
    nt_LWWU8._ipg_start += left;
    left = nt_LWWU8._ipg_start;
    right = nt_LWWU8._ipg_end;

    // { value = LWWU8.value }
    self.value = nt_LWWU8.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function LWWU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_TaggedU8;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // TaggedU8(2)[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU8 = TaggedU8(input, begin + left, begin + right, 2);
    if (nt_TaggedU8 === null) break _ipg_alt;
    if (nt_TaggedU8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU8._ipg_end);
    }
    nt_TaggedU8._ipg_end += left;
    nt_TaggedU8._ipg_start += left;
    left = nt_TaggedU8._ipg_start;
    right = nt_TaggedU8._ipg_end;

    // { value = lwwU8(TaggedId.value, TaggedU8.value) }
    self.value = lwwU8(nt_TaggedId.value, nt_TaggedU8.value);

    // ExpectEmpty[TaggedU8.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedU8._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function OptionalLWWFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWFloat32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWFloat32(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWFloat32 = LWWFloat32(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWFloat32 === null) break _ipg_alt;
    if (nt_LWWFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWFloat32._ipg_end);
    }
    nt_LWWFloat32._ipg_end += left;
    nt_LWWFloat32._ipg_start += left;
    left = nt_LWWFloat32._ipg_start;
    right = nt_LWWFloat32._ipg_end;

    // { value = LWWFloat32.value }
    self.value = nt_LWWFloat32.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function LWWFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_TaggedFloat32;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // TaggedFloat32(2)[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32 = TaggedFloat32(input, begin + left, begin + right, 2);
    if (nt_TaggedFloat32 === null) break _ipg_alt;
    if (nt_TaggedFloat32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32._ipg_end);
    }
    nt_TaggedFloat32._ipg_end += left;
    nt_TaggedFloat32._ipg_start += left;
    left = nt_TaggedFloat32._ipg_start;
    right = nt_TaggedFloat32._ipg_end;

    // { value = lwwFloat32(TaggedId.value, TaggedFloat32.value) }
    self.value = lwwFloat32(nt_TaggedId.value, nt_TaggedFloat32.value);

    // ExpectEmpty[TaggedFloat32.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedFloat32._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function OptionalLWWID(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWID;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWID(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWID = LWWID(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWID === null) break _ipg_alt;
    if (nt_LWWID._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWID._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWID._ipg_end);
    }
    nt_LWWID._ipg_end += left;
    nt_LWWID._ipg_start += left;
    left = nt_LWWID._ipg_start;
    right = nt_LWWID._ipg_end;

    // { value = LWWID.value }
    self.value = nt_LWWID.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function LWWID(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { timestamp = TaggedId.value }
    self.timestamp = nt_TaggedId.value;

    // TaggedId(2)[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { value = lwwCrdtId(timestamp, TaggedId.value) }
    self.value = lwwCrdtId(self.timestamp, nt_TaggedId.value);

    // ExpectEmpty[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function LWWString(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_TaggedId;
    let nt_String;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // TaggedId(1)[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // String(2)[TaggedId.END, SubBlock.END + SubBlock.length]
    left = nt_TaggedId._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String = String(input, begin + left, begin + right, 2);
    if (nt_String === null) break _ipg_alt;
    if (nt_String._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String._ipg_end);
    }
    nt_String._ipg_end += left;
    nt_String._ipg_start += left;
    left = nt_String._ipg_start;
    right = nt_String._ipg_end;

    // { value = lwwString(TaggedId.value, String.value) }
    self.value = lwwString(nt_TaggedId.value, nt_String.value);

    // ExpectEmpty[String.END, SubBlock.END + SubBlock.length]
    left = nt_String._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function String(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_VarUInt;
    let nt_Bool;
    let nt_Bytes;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // VarUInt[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // Bool[VarUInt.END, SubBlock.END + SubBlock.length]
    left = nt_VarUInt._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool = Bool(input, begin + left, begin + right);
    if (nt_Bool === null) break _ipg_alt;
    if (nt_Bool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool._ipg_end);
    }
    nt_Bool._ipg_end += left;
    nt_Bool._ipg_start += left;
    left = nt_Bool._ipg_start;
    right = nt_Bool._ipg_end;

    // ?[ check(Bool.value, "String flag unset") ]
    if (!check(nt_Bool.value, "String flag unset")) break _ipg_alt;

    // ?[ check(Bool.END + VarUInt.value <= SubBlock.END + SubBlock.length, "String: Overfull block") ]
    if (!check(nt_Bool._ipg_end + nt_VarUInt.value <= nt_SubBlock._ipg_end + nt_SubBlock.length, "String: Overfull block")) break _ipg_alt;

    // Bytes[Bool.END, Bool.END + VarUInt.value]
    left = nt_Bool._ipg_end;
    right = nt_Bool._ipg_end + nt_VarUInt.value;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes = Bytes(input, begin + left, begin + right);
    if (nt_Bytes === null) break _ipg_alt;
    if (nt_Bytes._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes._ipg_end);
    }
    nt_Bytes._ipg_end += left;
    nt_Bytes._ipg_start += left;
    left = nt_Bytes._ipg_start;
    right = nt_Bytes._ipg_end;

    // { value = decodeAscii(Bytes.value) }
    self.value = decodeAscii(nt_Bytes.value);

    // ExpectEmpty[Bytes.END, SubBlock.END + SubBlock.length]
    left = nt_Bytes._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function OptionalU32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedU32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedU32(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32 = TaggedU32(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedU32 === null) break _ipg_alt;
    if (nt_TaggedU32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32._ipg_end);
    }
    nt_TaggedU32._ipg_end += left;
    nt_TaggedU32._ipg_start += left;
    left = nt_TaggedU32._ipg_start;
    right = nt_TaggedU32._ipg_end;

    // { value = TaggedU32.value }
    self.value = nt_TaggedU32.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function OptionalIntPair(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_IntPair;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // IntPair(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_IntPair = IntPair(input, begin + left, begin + right, a_expectedIndex);
    if (nt_IntPair === null) break _ipg_alt;
    if (nt_IntPair._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_IntPair._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_IntPair._ipg_end);
    }
    nt_IntPair._ipg_end += left;
    nt_IntPair._ipg_start += left;
    left = nt_IntPair._ipg_start;
    right = nt_IntPair._ipg_end;

    // { value = IntPair.value }
    self.value = nt_IntPair.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function IntPair(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock;
    let nt_U32;
    let nt_ExpectEmpty;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock === null) break _ipg_alt;
    if (nt_SubBlock._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock._ipg_end);
    }
    nt_SubBlock._ipg_end += left;
    nt_SubBlock._ipg_start += left;
    left = nt_SubBlock._ipg_start;
    right = nt_SubBlock._ipg_end;

    // U32[SubBlock.END, SubBlock.END + SubBlock.length]
    left = nt_SubBlock._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32 = U32(input, begin + left, begin + right);
    if (nt_U32 === null) break _ipg_alt;
    if (nt_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32._ipg_end);
    }
    nt_U32._ipg_end += left;
    nt_U32._ipg_start += left;
    left = nt_U32._ipg_start;
    right = nt_U32._ipg_end;

    // { fst = U32.value }
    self.fst = nt_U32.value;

    // U32[U32.END, SubBlock.END + SubBlock.length]
    left = nt_U32._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32 = U32(input, begin + left, begin + right);
    if (nt_U32 === null) break _ipg_alt;
    if (nt_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32._ipg_end);
    }
    nt_U32._ipg_end += left;
    nt_U32._ipg_start += left;
    left = nt_U32._ipg_start;
    right = nt_U32._ipg_end;

    // { value = makePair(fst, U32.value) }
    self.value = makePair(self.fst, nt_U32.value);

    // ExpectEmpty[U32.END, SubBlock.END + SubBlock.length]
    left = nt_U32._ipg_end;
    right = nt_SubBlock._ipg_end + nt_SubBlock.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty === null) break _ipg_alt;
    if (nt_ExpectEmpty._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty._ipg_end);
    }
    nt_ExpectEmpty._ipg_end += left;
    nt_ExpectEmpty._ipg_start += left;
    left = nt_ExpectEmpty._ipg_start;
    right = nt_ExpectEmpty._ipg_end;

    return self;
  }

  
  return null;
}

function SubBlock(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Length4Tag;
    let nt_U32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Length4Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Length4Tag = Length4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Length4Tag === null) break _ipg_alt;
    if (nt_Length4Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Length4Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Length4Tag._ipg_end);
    }
    nt_Length4Tag._ipg_end += left;
    nt_Length4Tag._ipg_start += left;
    left = nt_Length4Tag._ipg_start;
    right = nt_Length4Tag._ipg_end;

    // U32[Length4Tag.END, EOI]
    left = nt_Length4Tag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32 = U32(input, begin + left, begin + right);
    if (nt_U32 === null) break _ipg_alt;
    if (nt_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32._ipg_end);
    }
    nt_U32._ipg_end += left;
    nt_U32._ipg_start += left;
    left = nt_U32._ipg_start;
    right = nt_U32._ipg_end;

    // { length = U32.value }
    self.length = nt_U32.value;

    return self;
  }

  
  return null;
}

function CrdtId(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { part1 = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.part1 = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // VarUInt[1, EOI]
    left = 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // { part2 = VarUInt.value }
    self.part2 = nt_VarUInt.value;

    return self;
  }

  
  return null;
}

function OptionalTaggedBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedBool;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedBool(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool = TaggedBool(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedBool === null) break _ipg_alt;
    if (nt_TaggedBool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool._ipg_end);
    }
    nt_TaggedBool._ipg_end += left;
    nt_TaggedBool._ipg_start += left;
    left = nt_TaggedBool._ipg_start;
    right = nt_TaggedBool._ipg_end;

    // { value = TaggedBool.value }
    self.value = nt_TaggedBool.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = !(!0) }
    self.value = !(!0);

    return self;
  }

  
  return null;
}

function TaggedBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte1Tag;
    let nt_Bool;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte1Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte1Tag = Byte1Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte1Tag === null) break _ipg_alt;
    if (nt_Byte1Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte1Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte1Tag._ipg_end);
    }
    nt_Byte1Tag._ipg_end += left;
    nt_Byte1Tag._ipg_start += left;
    left = nt_Byte1Tag._ipg_start;
    right = nt_Byte1Tag._ipg_end;

    // Bool[Byte1Tag.END, EOI]
    left = nt_Byte1Tag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool = Bool(input, begin + left, begin + right);
    if (nt_Bool === null) break _ipg_alt;
    if (nt_Bool._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool._ipg_end);
    }
    nt_Bool._ipg_end += left;
    nt_Bool._ipg_start += left;
    left = nt_Bool._ipg_start;
    right = nt_Bool._ipg_end;

    // { value = Bool.value }
    self.value = nt_Bool.value;

    return self;
  }

  
  return null;
}

function TaggedU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte1Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte1Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte1Tag = Byte1Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte1Tag === null) break _ipg_alt;
    if (nt_Byte1Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte1Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte1Tag._ipg_end);
    }
    nt_Byte1Tag._ipg_end += left;
    nt_Byte1Tag._ipg_start += left;
    left = nt_Byte1Tag._ipg_start;
    right = nt_Byte1Tag._ipg_end;

    // { value = .[Byte1Tag.END] }
    left = nt_Byte1Tag._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

function TaggedU32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte4Tag;
    let nt_U32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte4Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte4Tag = Byte4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte4Tag === null) break _ipg_alt;
    if (nt_Byte4Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte4Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte4Tag._ipg_end);
    }
    nt_Byte4Tag._ipg_end += left;
    nt_Byte4Tag._ipg_start += left;
    left = nt_Byte4Tag._ipg_start;
    right = nt_Byte4Tag._ipg_end;

    // U32[Byte4Tag.END, EOI]
    left = nt_Byte4Tag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32 = U32(input, begin + left, begin + right);
    if (nt_U32 === null) break _ipg_alt;
    if (nt_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32._ipg_end);
    }
    nt_U32._ipg_end += left;
    nt_U32._ipg_start += left;
    left = nt_U32._ipg_start;
    right = nt_U32._ipg_end;

    // { value = U32.value }
    self.value = nt_U32.value;

    return self;
  }

  
  return null;
}

function TaggedFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte4Tag;
    let nt_Float32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte4Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte4Tag = Byte4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte4Tag === null) break _ipg_alt;
    if (nt_Byte4Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte4Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte4Tag._ipg_end);
    }
    nt_Byte4Tag._ipg_end += left;
    nt_Byte4Tag._ipg_start += left;
    left = nt_Byte4Tag._ipg_start;
    right = nt_Byte4Tag._ipg_end;

    // Float32[Byte4Tag.END, EOI]
    left = nt_Byte4Tag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32 = Float32(input, begin + left, begin + right);
    if (nt_Float32 === null) break _ipg_alt;
    if (nt_Float32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32._ipg_end);
    }
    nt_Float32._ipg_end += left;
    nt_Float32._ipg_start += left;
    left = nt_Float32._ipg_start;
    right = nt_Float32._ipg_end;

    // { value = Float32.value }
    self.value = nt_Float32.value;

    return self;
  }

  
  return null;
}

function TaggedFloat64(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte8Tag;
    let nt_Float64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte8Tag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte8Tag = Byte8Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte8Tag === null) break _ipg_alt;
    if (nt_Byte8Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte8Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte8Tag._ipg_end);
    }
    nt_Byte8Tag._ipg_end += left;
    nt_Byte8Tag._ipg_start += left;
    left = nt_Byte8Tag._ipg_start;
    right = nt_Byte8Tag._ipg_end;

    // Float64[Byte8Tag.END, EOI]
    left = nt_Byte8Tag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64 = Float64(input, begin + left, begin + right);
    if (nt_Float64 === null) break _ipg_alt;
    if (nt_Float64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64._ipg_end);
    }
    nt_Float64._ipg_end += left;
    nt_Float64._ipg_start += left;
    left = nt_Float64._ipg_start;
    right = nt_Float64._ipg_end;

    // { value = Float64.value }
    self.value = nt_Float64.value;

    return self;
  }

  
  return null;
}

function OptionalTaggedId(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId = TaggedId(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedId === null) break _ipg_alt;
    if (nt_TaggedId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId._ipg_end);
    }
    nt_TaggedId._ipg_end += left;
    nt_TaggedId._ipg_start += left;
    left = nt_TaggedId._ipg_start;
    right = nt_TaggedId._ipg_end;

    // { value = TaggedId.value }
    self.value = nt_TaggedId.value;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

    return self;
  }

  
  return null;
}

function TaggedId(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_IDTag;
    let nt_CrdtId;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // IDTag(expectedIndex)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_IDTag = IDTag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_IDTag === null) break _ipg_alt;
    if (nt_IDTag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_IDTag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_IDTag._ipg_end);
    }
    nt_IDTag._ipg_end += left;
    nt_IDTag._ipg_start += left;
    left = nt_IDTag._ipg_start;
    right = nt_IDTag._ipg_end;

    // CrdtId[IDTag.END, EOI]
    left = nt_IDTag._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CrdtId = CrdtId(input, begin + left, begin + right);
    if (nt_CrdtId === null) break _ipg_alt;
    if (nt_CrdtId._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CrdtId._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CrdtId._ipg_end);
    }
    nt_CrdtId._ipg_end += left;
    nt_CrdtId._ipg_start += left;
    left = nt_CrdtId._ipg_start;
    right = nt_CrdtId._ipg_end;

    // { value = makeCrdtId(CrdtId.part1, CrdtId.part2) }
    self.value = makeCrdtId(nt_CrdtId.part1, nt_CrdtId.part2);

    return self;
  }

  
  return null;
}

function IDTag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag(expectedIndex, 15)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag = Tag(input, begin + left, begin + right, a_expectedIndex, 15);
    if (nt_Tag === null) break _ipg_alt;
    if (nt_Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag._ipg_end);
    }
    nt_Tag._ipg_end += left;
    nt_Tag._ipg_start += left;
    left = nt_Tag._ipg_start;
    right = nt_Tag._ipg_end;

    return self;
  }

  
  return null;
}

function Byte1Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag(expectedIndex, 1)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag = Tag(input, begin + left, begin + right, a_expectedIndex, 1);
    if (nt_Tag === null) break _ipg_alt;
    if (nt_Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag._ipg_end);
    }
    nt_Tag._ipg_end += left;
    nt_Tag._ipg_start += left;
    left = nt_Tag._ipg_start;
    right = nt_Tag._ipg_end;

    return self;
  }

  
  return null;
}

function Byte4Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag(expectedIndex, 4)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag = Tag(input, begin + left, begin + right, a_expectedIndex, 4);
    if (nt_Tag === null) break _ipg_alt;
    if (nt_Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag._ipg_end);
    }
    nt_Tag._ipg_end += left;
    nt_Tag._ipg_start += left;
    left = nt_Tag._ipg_start;
    right = nt_Tag._ipg_end;

    return self;
  }

  
  return null;
}

function Byte8Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag(expectedIndex, 8)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag = Tag(input, begin + left, begin + right, a_expectedIndex, 8);
    if (nt_Tag === null) break _ipg_alt;
    if (nt_Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag._ipg_end);
    }
    nt_Tag._ipg_end += left;
    nt_Tag._ipg_start += left;
    left = nt_Tag._ipg_start;
    right = nt_Tag._ipg_end;

    return self;
  }

  
  return null;
}

function Length4Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag(expectedIndex, 12)[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag = Tag(input, begin + left, begin + right, a_expectedIndex, 12);
    if (nt_Tag === null) break _ipg_alt;
    if (nt_Tag._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag._ipg_end);
    }
    nt_Tag._ipg_end += left;
    nt_Tag._ipg_start += left;
    left = nt_Tag._ipg_start;
    right = nt_Tag._ipg_end;

    return self;
  }

  
  return null;
}

function Tag(input, begin = 0, end = input.length, a_expectedIndex, a_expectedTagType) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VarUInt[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // ?[ expectedIndex == VarUInt.value >> 4 ]
    if (!(a_expectedIndex == nt_VarUInt.value >> 4)) break _ipg_alt;

    // ?[ expectedTagType == (VarUInt.value & 15) ]
    if (!(a_expectedTagType == (nt_VarUInt.value & 15))) break _ipg_alt;

    return self;
  }

  
  return null;
}

function ExpectEmpty(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ warnIf(EOI != 0, "Underfull block") ]
    if (!warnIf(EOI != 0, "Underfull block")) break _ipg_alt;

    // { _ = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self._ = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

function Bytes(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.value = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

function Bool(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00"[0, 1]
    left = 0;
    right = 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { value = !(!0) }
    self.value = !(!0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x01"[0, 1]
    left = 0;
    right = 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    // { value = !0 }
    self.value = !0;

    return self;
  }

  
  return null;
}

function U8(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

function U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 2] }
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
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

function U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 4] }
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = bs[0] | bs[1] << 8 | bs[2] << 16 | bs[3] << 24 }
    self.value = self.bs[0] | self.bs[1] << 8 | self.bs[2] << 16 | self.bs[3] << 24;

    return self;
  }

  
  return null;
}

function Float32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 4] }
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = toFloat32(bs) }
    self.value = toFloat32(self.bs);

    return self;
  }

  
  return null;
}

function Float64(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 8] }
    left = 0;
    right = 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = toFloat64(bs) }
    self.value = toFloat64(self.bs);

    return self;
  }

  
  return null;
}

function VarUInt(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ value >> 7 == 0 ]
    if (!(self.value >> 7 == 0)) break _ipg_alt;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U8;
    let nt_VarUInt;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U8[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U8 = U8(input, begin + left, begin + right);
    if (nt_U8 === null) break _ipg_alt;
    if (nt_U8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8._ipg_end);
    }
    nt_U8._ipg_end += left;
    nt_U8._ipg_start += left;
    left = nt_U8._ipg_start;
    right = nt_U8._ipg_end;

    // VarUInt[U8.END, EOI]
    left = nt_U8._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt === null) break _ipg_alt;
    if (nt_VarUInt._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt._ipg_end);
    }
    nt_VarUInt._ipg_end += left;
    nt_VarUInt._ipg_start += left;
    left = nt_VarUInt._ipg_start;
    right = nt_VarUInt._ipg_end;

    // { value = U8.value & 127 | VarUInt.value << 7 }
    self.value = nt_U8.value & 127 | nt_VarUInt.value << 7;

    return self;
  }

  
  return null;
}

console.log(JSON.stringify(RM6(fs.readFileSync("./text_example.rm"))));
