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
const _ipg_failTreeRoot = { children: [] };
const _ipg_failTreeStack = [_ipg_failTreeRoot];
export function RM6(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "RM6",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_HeaderV6_0;
    let nt_Blocks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // HeaderV6@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "HeaderV6@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HeaderV6_0 = HeaderV6(input, begin + left, begin + right);
    if (nt_HeaderV6_0 === null) break _ipg_alt;
    if (nt_HeaderV6_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HeaderV6_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HeaderV6_0._ipg_end);
    }
    nt_HeaderV6_0._ipg_end += left;
    nt_HeaderV6_0._ipg_start += left;
    left = nt_HeaderV6_0._ipg_start;
    right = nt_HeaderV6_0._ipg_end;

    // Blocks@0[HeaderV6@0.END, EOI]
    left = nt_HeaderV6_0._ipg_end;
    _ipg_failedTerm = { term: "Blocks@0[HeaderV6@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Blocks_0 = Blocks(input, begin + left, begin + right);
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

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function HeaderV6(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "HeaderV6",
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

    // "reMarkable .lines file, version=6          "[0, 43]
    left = 0;
    right = 43;
    _ipg_failedTerm = { term: "\"reMarkable .lines file, version=6          \"[0, 43]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "reMarkable .lines file, version=6          ")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 43;
    self._ipg_end = Math.max(self._ipg_end, right);

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
    let nt_FullBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat FullBlock@0[FullBlock@0.END, EOI].block starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_FullBlock_0 = FullBlock(input, begin + left, begin + right);
    if (nt_FullBlock_0 !== null) {
      if (nt_FullBlock_0._ipg_end === 0) throw 'repeat of non-consuming rule: FullBlock';
      self._ipg_start = Math.min(self._ipg_start, left + nt_FullBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_FullBlock_0._ipg_end);
      nt_FullBlock_0._ipg_end += left;
      nt_FullBlock_0._ipg_start += left;
      left = nt_FullBlock_0._ipg_end;
      right = EOI;
      self.values.push(nt_FullBlock_0.block);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_FullBlock_0 = FullBlock(input, begin + left, begin + right);
        if (nt_FullBlock_0 === null) break;
        if (nt_FullBlock_0._ipg_end === 0) throw 'repeat of non-consuming rule: FullBlock';
        self._ipg_start = Math.min(self._ipg_start, left + nt_FullBlock_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_FullBlock_0._ipg_end);
        nt_FullBlock_0._ipg_end += left;
        nt_FullBlock_0._ipg_start += left;
        self.values.push(nt_FullBlock_0.block);
        left = nt_FullBlock_0._ipg_end;
        right = EOI;
      }
    }

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function FullBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "FullBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MainBlockInfo_0;
    let nt_Block_0;
    let nt_Bytes_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // MainBlockInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "MainBlockInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MainBlockInfo_0 = MainBlockInfo(input, begin + left, begin + right);
    if (nt_MainBlockInfo_0 === null) break _ipg_alt;
    if (nt_MainBlockInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MainBlockInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MainBlockInfo_0._ipg_end);
    }
    nt_MainBlockInfo_0._ipg_end += left;
    nt_MainBlockInfo_0._ipg_start += left;
    left = nt_MainBlockInfo_0._ipg_start;
    right = nt_MainBlockInfo_0._ipg_end;

    // Block@0(MainBlockInfo@0.blockType, MainBlockInfo@0.currentVersion)[MainBlockInfo@0.END, MainBlockInfo@0.END + MainBlockInfo@0.length]
    left = nt_MainBlockInfo_0._ipg_end;
    _ipg_failedTerm = { term: "Block@0(MainBlockInfo@0.blockType, MainBlockInfo@0.currentVersion)[MainBlockInfo@0.END, MainBlockInfo@0.END + MainBlockInfo@0.length]", left, right };
    right = nt_MainBlockInfo_0._ipg_end + nt_MainBlockInfo_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Block_0 = Block(input, begin + left, begin + right, nt_MainBlockInfo_0.blockType, nt_MainBlockInfo_0.currentVersion);
    if (nt_Block_0 === null) break _ipg_alt;
    if (nt_Block_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Block_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Block_0._ipg_end);
    }
    nt_Block_0._ipg_end += left;
    nt_Block_0._ipg_start += left;
    left = nt_Block_0._ipg_start;
    right = nt_Block_0._ipg_end;

    // Bytes@0[Block@0.END, MainBlockInfo@0.END + MainBlockInfo@0.length]
    left = nt_Block_0._ipg_end;
    _ipg_failedTerm = { term: "Bytes@0[Block@0.END, MainBlockInfo@0.END + MainBlockInfo@0.length]", left, right };
    right = nt_MainBlockInfo_0._ipg_end + nt_MainBlockInfo_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes_0 = Bytes(input, begin + left, begin + right);
    if (nt_Bytes_0 === null) break _ipg_alt;
    if (nt_Bytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes_0._ipg_end);
    }
    nt_Bytes_0._ipg_end += left;
    nt_Bytes_0._ipg_start += left;
    left = nt_Bytes_0._ipg_start;
    right = nt_Bytes_0._ipg_end;

    // { block = makeBlock(MainBlockInfo@0.this, Block@0.data, Bytes@0.value) }
    self.block = makeBlock((({_ipg_start,_ipg_end,...o}) => o)(nt_MainBlockInfo_0), nt_Block_0.data, nt_Bytes_0.value);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function MainBlockInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "MainBlockInfo",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32_0;
    let nt_U8_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U32@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "U32@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_0 = U32(input, begin + left, begin + right);
    if (nt_U32_0 === null) break _ipg_alt;
    if (nt_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_0._ipg_end);
    }
    nt_U32_0._ipg_end += left;
    nt_U32_0._ipg_start += left;
    left = nt_U32_0._ipg_start;
    right = nt_U32_0._ipg_end;

    // { length = U32@0.value }
    self.length = nt_U32_0.value;

    // U8@0[U32@0.END, EOI]
    left = nt_U32_0._ipg_end;
    _ipg_failedTerm = { term: "U8@0[U32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U8_0 = U8(input, begin + left, begin + right);
    if (nt_U8_0 === null) break _ipg_alt;
    if (nt_U8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8_0._ipg_end);
    }
    nt_U8_0._ipg_end += left;
    nt_U8_0._ipg_start += left;
    left = nt_U8_0._ipg_start;
    right = nt_U8_0._ipg_end;

    // ?[ check(U8@0.value == 0, "unknown value is non-zero") ]
    _ipg_failedTerm = { term: "?[ check(U8@0.value == 0, \"unknown value is non-zero\") ]" };
    if (!check(nt_U8_0.value == 0, "unknown value is non-zero")) break _ipg_alt;

    // { minVersion = .[U8@0.END] }
    left = nt_U8_0._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ minVersion = .[U8@0.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.minVersion = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ check(minVersion >= 0, "minVersion < 0") ]
    _ipg_failedTerm = { term: "?[ check(minVersion >= 0, \"minVersion < 0\") ]" };
    if (!check(self.minVersion >= 0, "minVersion < 0")) break _ipg_alt;

    // { currentVersion = .[U8@0.END + 1] }
    left = nt_U8_0._ipg_end + 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ currentVersion = .[U8@0.END + 1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.currentVersion = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ check(currentVersion >= 0 && minVersion <= currentVersion, "currentVersion not between 0 and minVersion") ]
    _ipg_failedTerm = { term: "?[ check(currentVersion >= 0 && minVersion <= currentVersion, \"currentVersion not between 0 and minVersion\") ]" };
    if (!check(self.currentVersion >= 0 && self.minVersion <= self.currentVersion, "currentVersion not between 0 and minVersion")) break _ipg_alt;

    // { blockType = .[U8@0.END + 2] }
    left = nt_U8_0._ipg_end + 2;
    right = left + 1;
    _ipg_failedTerm = { term: "{ blockType = .[U8@0.END + 2] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.blockType = input[begin + left];
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

function Block(input, begin = 0, end = input.length, a_blockType, a_version) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Block",
    args: [a_blockType, a_version],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MigrationInfoBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 0 ]
    _ipg_failedTerm = { term: "?[ blockType == 0 ]" };
    if (!(a_blockType == 0)) break _ipg_alt;

    // MigrationInfoBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "MigrationInfoBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_MigrationInfoBlock_0 = MigrationInfoBlock(input, begin + left, begin + right);
    if (nt_MigrationInfoBlock_0 === null) break _ipg_alt;
    if (nt_MigrationInfoBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_MigrationInfoBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MigrationInfoBlock_0._ipg_end);
    }
    nt_MigrationInfoBlock_0._ipg_end += left;
    nt_MigrationInfoBlock_0._ipg_start += left;
    left = nt_MigrationInfoBlock_0._ipg_start;
    right = nt_MigrationInfoBlock_0._ipg_end;

    // { data = MigrationInfoBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_MigrationInfoBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTreeBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 1 ]
    _ipg_failedTerm = { term: "?[ blockType == 1 ]" };
    if (!(a_blockType == 1)) break _ipg_alt;

    // SceneTreeBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneTreeBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTreeBlock_0 = SceneTreeBlock(input, begin + left, begin + right);
    if (nt_SceneTreeBlock_0 === null) break _ipg_alt;
    if (nt_SceneTreeBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTreeBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTreeBlock_0._ipg_end);
    }
    nt_SceneTreeBlock_0._ipg_end += left;
    nt_SceneTreeBlock_0._ipg_start += left;
    left = nt_SceneTreeBlock_0._ipg_start;
    right = nt_SceneTreeBlock_0._ipg_end;

    // { data = SceneTreeBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTreeBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TreeNodeBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 2 ]
    _ipg_failedTerm = { term: "?[ blockType == 2 ]" };
    if (!(a_blockType == 2)) break _ipg_alt;

    // TreeNodeBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TreeNodeBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TreeNodeBlock_0 = TreeNodeBlock(input, begin + left, begin + right);
    if (nt_TreeNodeBlock_0 === null) break _ipg_alt;
    if (nt_TreeNodeBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TreeNodeBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TreeNodeBlock_0._ipg_end);
    }
    nt_TreeNodeBlock_0._ipg_end += left;
    nt_TreeNodeBlock_0._ipg_start += left;
    left = nt_TreeNodeBlock_0._ipg_start;
    right = nt_TreeNodeBlock_0._ipg_end;

    // { data = TreeNodeBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_TreeNodeBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneGlyphItemBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 3 ]
    _ipg_failedTerm = { term: "?[ blockType == 3 ]" };
    if (!(a_blockType == 3)) break _ipg_alt;

    // SceneGlyphItemBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneGlyphItemBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGlyphItemBlock_0 = SceneGlyphItemBlock(input, begin + left, begin + right);
    if (nt_SceneGlyphItemBlock_0 === null) break _ipg_alt;
    if (nt_SceneGlyphItemBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGlyphItemBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGlyphItemBlock_0._ipg_end);
    }
    nt_SceneGlyphItemBlock_0._ipg_end += left;
    nt_SceneGlyphItemBlock_0._ipg_start += left;
    left = nt_SceneGlyphItemBlock_0._ipg_start;
    right = nt_SceneGlyphItemBlock_0._ipg_end;

    // { data = SceneGlyphItemBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneGlyphItemBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneGroupItemBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 4 ]
    _ipg_failedTerm = { term: "?[ blockType == 4 ]" };
    if (!(a_blockType == 4)) break _ipg_alt;

    // SceneGroupItemBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneGroupItemBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGroupItemBlock_0 = SceneGroupItemBlock(input, begin + left, begin + right);
    if (nt_SceneGroupItemBlock_0 === null) break _ipg_alt;
    if (nt_SceneGroupItemBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGroupItemBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGroupItemBlock_0._ipg_end);
    }
    nt_SceneGroupItemBlock_0._ipg_end += left;
    nt_SceneGroupItemBlock_0._ipg_start += left;
    left = nt_SceneGroupItemBlock_0._ipg_start;
    right = nt_SceneGroupItemBlock_0._ipg_end;

    // { data = SceneGroupItemBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneGroupItemBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneLineItemBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 5 ]
    _ipg_failedTerm = { term: "?[ blockType == 5 ]" };
    if (!(a_blockType == 5)) break _ipg_alt;

    // SceneLineItemBlock@0(version)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneLineItemBlock@0(version)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneLineItemBlock_0 = SceneLineItemBlock(input, begin + left, begin + right, a_version);
    if (nt_SceneLineItemBlock_0 === null) break _ipg_alt;
    if (nt_SceneLineItemBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneLineItemBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneLineItemBlock_0._ipg_end);
    }
    nt_SceneLineItemBlock_0._ipg_end += left;
    nt_SceneLineItemBlock_0._ipg_start += left;
    left = nt_SceneLineItemBlock_0._ipg_start;
    right = nt_SceneLineItemBlock_0._ipg_end;

    // { data = SceneLineItemBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneLineItemBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTextItemBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 6 ]
    _ipg_failedTerm = { term: "?[ blockType == 6 ]" };
    if (!(a_blockType == 6)) break _ipg_alt;

    // SceneTextItemBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneTextItemBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTextItemBlock_0 = SceneTextItemBlock(input, begin + left, begin + right);
    if (nt_SceneTextItemBlock_0 === null) break _ipg_alt;
    if (nt_SceneTextItemBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTextItemBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTextItemBlock_0._ipg_end);
    }
    nt_SceneTextItemBlock_0._ipg_end += left;
    nt_SceneTextItemBlock_0._ipg_start += left;
    left = nt_SceneTextItemBlock_0._ipg_start;
    right = nt_SceneTextItemBlock_0._ipg_end;

    // { data = SceneTextItemBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTextItemBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RootTextBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 7 ]
    _ipg_failedTerm = { term: "?[ blockType == 7 ]" };
    if (!(a_blockType == 7)) break _ipg_alt;

    // RootTextBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "RootTextBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RootTextBlock_0 = RootTextBlock(input, begin + left, begin + right);
    if (nt_RootTextBlock_0 === null) break _ipg_alt;
    if (nt_RootTextBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RootTextBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RootTextBlock_0._ipg_end);
    }
    nt_RootTextBlock_0._ipg_end += left;
    nt_RootTextBlock_0._ipg_start += left;
    left = nt_RootTextBlock_0._ipg_start;
    right = nt_RootTextBlock_0._ipg_end;

    // { data = RootTextBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_RootTextBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneTombstoneItemBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 8 ]
    _ipg_failedTerm = { term: "?[ blockType == 8 ]" };
    if (!(a_blockType == 8)) break _ipg_alt;

    // SceneTombstoneItemBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneTombstoneItemBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneTombstoneItemBlock_0 = SceneTombstoneItemBlock(input, begin + left, begin + right);
    if (nt_SceneTombstoneItemBlock_0 === null) break _ipg_alt;
    if (nt_SceneTombstoneItemBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneTombstoneItemBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneTombstoneItemBlock_0._ipg_end);
    }
    nt_SceneTombstoneItemBlock_0._ipg_end += left;
    nt_SceneTombstoneItemBlock_0._ipg_start += left;
    left = nt_SceneTombstoneItemBlock_0._ipg_start;
    right = nt_SceneTombstoneItemBlock_0._ipg_end;

    // { data = SceneTombstoneItemBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneTombstoneItemBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_AuthorIdsBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 9 ]
    _ipg_failedTerm = { term: "?[ blockType == 9 ]" };
    if (!(a_blockType == 9)) break _ipg_alt;

    // AuthorIdsBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "AuthorIdsBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AuthorIdsBlock_0 = AuthorIdsBlock(input, begin + left, begin + right);
    if (nt_AuthorIdsBlock_0 === null) break _ipg_alt;
    if (nt_AuthorIdsBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AuthorIdsBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AuthorIdsBlock_0._ipg_end);
    }
    nt_AuthorIdsBlock_0._ipg_end += left;
    nt_AuthorIdsBlock_0._ipg_start += left;
    left = nt_AuthorIdsBlock_0._ipg_start;
    right = nt_AuthorIdsBlock_0._ipg_end;

    // { data = AuthorIdsBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_AuthorIdsBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_PageInfoBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 10 ]
    _ipg_failedTerm = { term: "?[ blockType == 10 ]" };
    if (!(a_blockType == 10)) break _ipg_alt;

    // PageInfoBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "PageInfoBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PageInfoBlock_0 = PageInfoBlock(input, begin + left, begin + right);
    if (nt_PageInfoBlock_0 === null) break _ipg_alt;
    if (nt_PageInfoBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PageInfoBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PageInfoBlock_0._ipg_end);
    }
    nt_PageInfoBlock_0._ipg_end += left;
    nt_PageInfoBlock_0._ipg_start += left;
    left = nt_PageInfoBlock_0._ipg_start;
    right = nt_PageInfoBlock_0._ipg_end;

    // { data = PageInfoBlock@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_PageInfoBlock_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneInfo_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ blockType == 13 ]
    _ipg_failedTerm = { term: "?[ blockType == 13 ]" };
    if (!(a_blockType == 13)) break _ipg_alt;

    // SceneInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneInfo_0 = SceneInfo(input, begin + left, begin + right);
    if (nt_SceneInfo_0 === null) break _ipg_alt;
    if (nt_SceneInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneInfo_0._ipg_end);
    }
    nt_SceneInfo_0._ipg_end += left;
    nt_SceneInfo_0._ipg_start += left;
    left = nt_SceneInfo_0._ipg_start;
    right = nt_SceneInfo_0._ipg_end;

    // { data = SceneInfo@0.this }
    self.data = (({_ipg_start,_ipg_end,...o}) => o)(nt_SceneInfo_0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_UnknownBlock_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // UnknownBlock@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "UnknownBlock@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_UnknownBlock_0 = UnknownBlock(input, begin + left, begin + right);
    if (nt_UnknownBlock_0 === null) break _ipg_alt;
    if (nt_UnknownBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_UnknownBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_UnknownBlock_0._ipg_end);
    }
    nt_UnknownBlock_0._ipg_end += left;
    nt_UnknownBlock_0._ipg_start += left;
    left = nt_UnknownBlock_0._ipg_start;
    right = nt_UnknownBlock_0._ipg_end;

    // { data = UnknownBlock@0.data }
    self.data = nt_UnknownBlock_0.data;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function AuthorIdsBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "AuthorIdsBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt_0;
    let nt_AuthorId_0;
    let seq_AuthorId_0; let seq_AuthorId_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VarUInt@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "VarUInt@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // for i = 0 to VarUInt@0.value do AuthorId@0[AuthorId@0.END, EOI]
    _ipg_failedTerm = { term: "for i = 0 to VarUInt@0.value do AuthorId@0[AuthorId@0.END, EOI]" };
    nt_AuthorId_0 = { _ipg_end: right, _ipg_start: left };
    seq_AuthorId_0_start = 0;
    loopEnd = nt_VarUInt_0.value;
    seq_AuthorId_0 = new Array(Math.max(0, loopEnd - seq_AuthorId_0_start));
    for (let i_i = seq_AuthorId_0_start; i_i < loopEnd; i_i++) {
      const left = nt_AuthorId_0._ipg_end;
      const right = EOI;
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = AuthorId(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_AuthorId_0._ipg_end = tmp._ipg_end;
      nt_AuthorId_0._ipg_start = tmp._ipg_start;
      seq_AuthorId_0[i_i - seq_AuthorId_0_start] = tmp;
    }
    left = nt_AuthorId_0._ipg_start;
    right = nt_AuthorId_0._ipg_end;

    // { authorIds = AuthorId@0.these }
    self.authorIds = seq_AuthorId_0.map(({_ipg_start,_ipg_end,...o}) => o);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function AuthorId(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "AuthorId",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_VarUInt_0;
    let nt_U16_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(0)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(0)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 0);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // ?[ check(VarUInt@0.value == 16, "Expect UUID to have length 16") ]
    _ipg_failedTerm = { term: "?[ check(VarUInt@0.value == 16, \"Expect UUID to have length 16\") ]" };
    if (!check(nt_VarUInt_0.value == 16, "Expect UUID to have length 16")) break _ipg_alt;

    // { uuid = *[VarUInt@0.END, VarUInt@0.END + VarUInt@0.value] }
    left = nt_VarUInt_0._ipg_end;
    right = nt_VarUInt_0._ipg_end + nt_VarUInt_0.value;
    _ipg_failedTerm = { term: "{ uuid = *[VarUInt@0.END, VarUInt@0.END + VarUInt@0.value] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.uuid = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // U16@0[VarUInt@0.END + VarUInt@0.value, SubBlock@0.END + SubBlock@0.length]
    left = nt_VarUInt_0._ipg_end + nt_VarUInt_0.value;
    _ipg_failedTerm = { term: "U16@0[VarUInt@0.END + VarUInt@0.value, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { authorId = U16@0.value }
    self.authorId = nt_U16_0.value;

    // ExpectEmpty@0[U16@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_U16_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[U16@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function MigrationInfoBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "MigrationInfoBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    let nt_TaggedBool_0;
    let nt_OptionalTaggedBool_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { migrationId = TaggedId@0.value }
    self.migrationId = nt_TaggedId_0.value;

    // TaggedBool@0(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedBool@0(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool_0 = TaggedBool(input, begin + left, begin + right, 2);
    if (nt_TaggedBool_0 === null) break _ipg_alt;
    if (nt_TaggedBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool_0._ipg_end);
    }
    nt_TaggedBool_0._ipg_end += left;
    nt_TaggedBool_0._ipg_start += left;
    left = nt_TaggedBool_0._ipg_start;
    right = nt_TaggedBool_0._ipg_end;

    // { isDevice = TaggedBool@0.value }
    self.isDevice = nt_TaggedBool_0.value;

    // OptionalTaggedBool@0(3)[TaggedBool@0.END, EOI]
    left = nt_TaggedBool_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalTaggedBool@0(3)[TaggedBool@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalTaggedBool_0 = OptionalTaggedBool(input, begin + left, begin + right, 3);
    if (nt_OptionalTaggedBool_0 === null) break _ipg_alt;
    if (nt_OptionalTaggedBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalTaggedBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalTaggedBool_0._ipg_end);
    }
    nt_OptionalTaggedBool_0._ipg_end += left;
    nt_OptionalTaggedBool_0._ipg_start += left;
    left = nt_OptionalTaggedBool_0._ipg_start;
    right = nt_OptionalTaggedBool_0._ipg_end;

    // { unknown = OptionalTaggedBool@0.value }
    self.unknown = nt_OptionalTaggedBool_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TreeNodeBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TreeNodeBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    let nt_LWWString_0;
    let nt_LWWBool_0;
    let nt_OptionalLWWID_0;
    let nt_OptionalLWWU8_0;
    let nt_OptionalLWWFloat32_0;
    let nt_OptionalLWWFloat32_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { nodeId = TaggedId@0.value }
    self.nodeId = nt_TaggedId_0.value;

    // LWWString@0(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "LWWString@0(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWString_0 = LWWString(input, begin + left, begin + right, 2);
    if (nt_LWWString_0 === null) break _ipg_alt;
    if (nt_LWWString_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWString_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWString_0._ipg_end);
    }
    nt_LWWString_0._ipg_end += left;
    nt_LWWString_0._ipg_start += left;
    left = nt_LWWString_0._ipg_start;
    right = nt_LWWString_0._ipg_end;

    // { label = LWWString@0.value }
    self.label = nt_LWWString_0.value;

    // LWWBool@0(3)[LWWString@0.END, EOI]
    left = nt_LWWString_0._ipg_end;
    _ipg_failedTerm = { term: "LWWBool@0(3)[LWWString@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWBool_0 = LWWBool(input, begin + left, begin + right, 3);
    if (nt_LWWBool_0 === null) break _ipg_alt;
    if (nt_LWWBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWBool_0._ipg_end);
    }
    nt_LWWBool_0._ipg_end += left;
    nt_LWWBool_0._ipg_start += left;
    left = nt_LWWBool_0._ipg_start;
    right = nt_LWWBool_0._ipg_end;

    // { visible = LWWBool@0.value }
    self.visible = nt_LWWBool_0.value;

    // OptionalLWWID@0(7)[LWWBool@0.END, EOI]
    left = nt_LWWBool_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWID@0(7)[LWWBool@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWID_0 = OptionalLWWID(input, begin + left, begin + right, 7);
    if (nt_OptionalLWWID_0 === null) break _ipg_alt;
    if (nt_OptionalLWWID_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWID_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWID_0._ipg_end);
    }
    nt_OptionalLWWID_0._ipg_end += left;
    nt_OptionalLWWID_0._ipg_start += left;
    left = nt_OptionalLWWID_0._ipg_start;
    right = nt_OptionalLWWID_0._ipg_end;

    // { anchorId = OptionalLWWID@0.value }
    self.anchorId = nt_OptionalLWWID_0.value;

    // OptionalLWWU8@0(8)[OptionalLWWID@0.END, EOI]
    left = nt_OptionalLWWID_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWU8@0(8)[OptionalLWWID@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWU8_0 = OptionalLWWU8(input, begin + left, begin + right, 8);
    if (nt_OptionalLWWU8_0 === null) break _ipg_alt;
    if (nt_OptionalLWWU8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWU8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWU8_0._ipg_end);
    }
    nt_OptionalLWWU8_0._ipg_end += left;
    nt_OptionalLWWU8_0._ipg_start += left;
    left = nt_OptionalLWWU8_0._ipg_start;
    right = nt_OptionalLWWU8_0._ipg_end;

    // { anchorType = OptionalLWWU8@0.value }
    self.anchorType = nt_OptionalLWWU8_0.value;

    // OptionalLWWFloat32@0(9)[OptionalLWWU8@0.END, EOI]
    left = nt_OptionalLWWU8_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWFloat32@0(9)[OptionalLWWU8@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWFloat32_0 = OptionalLWWFloat32(input, begin + left, begin + right, 9);
    if (nt_OptionalLWWFloat32_0 === null) break _ipg_alt;
    if (nt_OptionalLWWFloat32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWFloat32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWFloat32_0._ipg_end);
    }
    nt_OptionalLWWFloat32_0._ipg_end += left;
    nt_OptionalLWWFloat32_0._ipg_start += left;
    left = nt_OptionalLWWFloat32_0._ipg_start;
    right = nt_OptionalLWWFloat32_0._ipg_end;

    // { anchorThreshold = OptionalLWWFloat32@0.value }
    self.anchorThreshold = nt_OptionalLWWFloat32_0.value;

    // OptionalLWWFloat32@1(10)[OptionalLWWFloat32@0.END, EOI]
    left = nt_OptionalLWWFloat32_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWFloat32@1(10)[OptionalLWWFloat32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWFloat32_1 = OptionalLWWFloat32(input, begin + left, begin + right, 10);
    if (nt_OptionalLWWFloat32_1 === null) break _ipg_alt;
    if (nt_OptionalLWWFloat32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWFloat32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWFloat32_1._ipg_end);
    }
    nt_OptionalLWWFloat32_1._ipg_end += left;
    nt_OptionalLWWFloat32_1._ipg_start += left;
    left = nt_OptionalLWWFloat32_1._ipg_start;
    right = nt_OptionalLWWFloat32_1._ipg_end;

    // { anchorOriginX = OptionalLWWFloat32@1.value }
    self.anchorOriginX = nt_OptionalLWWFloat32_1.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function PageInfoBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "PageInfoBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedU32_0;
    let nt_TaggedU32_1;
    let nt_TaggedU32_2;
    let nt_TaggedU32_3;
    let nt_OptionalU32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedU32@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedU32@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, 1);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { loadsCount = TaggedU32@0.value }
    self.loadsCount = nt_TaggedU32_0.value;

    // TaggedU32@1(2)[TaggedU32@0.END, EOI]
    left = nt_TaggedU32_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@1(2)[TaggedU32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_1 = TaggedU32(input, begin + left, begin + right, 2);
    if (nt_TaggedU32_1 === null) break _ipg_alt;
    if (nt_TaggedU32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_1._ipg_end);
    }
    nt_TaggedU32_1._ipg_end += left;
    nt_TaggedU32_1._ipg_start += left;
    left = nt_TaggedU32_1._ipg_start;
    right = nt_TaggedU32_1._ipg_end;

    // { mergesCount = TaggedU32@1.value }
    self.mergesCount = nt_TaggedU32_1.value;

    // TaggedU32@2(3)[TaggedU32@1.END, EOI]
    left = nt_TaggedU32_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@2(3)[TaggedU32@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_2 = TaggedU32(input, begin + left, begin + right, 3);
    if (nt_TaggedU32_2 === null) break _ipg_alt;
    if (nt_TaggedU32_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_2._ipg_end);
    }
    nt_TaggedU32_2._ipg_end += left;
    nt_TaggedU32_2._ipg_start += left;
    left = nt_TaggedU32_2._ipg_start;
    right = nt_TaggedU32_2._ipg_end;

    // { textCharsCount = TaggedU32@2.value }
    self.textCharsCount = nt_TaggedU32_2.value;

    // TaggedU32@3(4)[TaggedU32@2.END, EOI]
    left = nt_TaggedU32_2._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@3(4)[TaggedU32@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_3 = TaggedU32(input, begin + left, begin + right, 4);
    if (nt_TaggedU32_3 === null) break _ipg_alt;
    if (nt_TaggedU32_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_3._ipg_end);
    }
    nt_TaggedU32_3._ipg_end += left;
    nt_TaggedU32_3._ipg_start += left;
    left = nt_TaggedU32_3._ipg_start;
    right = nt_TaggedU32_3._ipg_end;

    // { textLinesCount = TaggedU32@3.value }
    self.textLinesCount = nt_TaggedU32_3.value;

    // OptionalU32@0(5)[TaggedU32@3.END, EOI]
    left = nt_TaggedU32_3._ipg_end;
    _ipg_failedTerm = { term: "OptionalU32@0(5)[TaggedU32@3.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32_0 = OptionalU32(input, begin + left, begin + right, 5);
    if (nt_OptionalU32_0 === null) break _ipg_alt;
    if (nt_OptionalU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32_0._ipg_end);
    }
    nt_OptionalU32_0._ipg_end += left;
    nt_OptionalU32_0._ipg_start += left;
    left = nt_OptionalU32_0._ipg_start;
    right = nt_OptionalU32_0._ipg_end;

    // { typeFolioUseCount = OptionalU32@0.value }
    self.typeFolioUseCount = nt_OptionalU32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneTreeBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneTreeBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    let nt_TaggedId_1;
    let nt_TaggedBool_0;
    let nt_SubBlock_0;
    let nt_TaggedId_2;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { treeId = TaggedId@0.value }
    self.treeId = nt_TaggedId_0.value;

    // TaggedId@1(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@1(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_1 = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId_1 === null) break _ipg_alt;
    if (nt_TaggedId_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_1._ipg_end);
    }
    nt_TaggedId_1._ipg_end += left;
    nt_TaggedId_1._ipg_start += left;
    left = nt_TaggedId_1._ipg_start;
    right = nt_TaggedId_1._ipg_end;

    // { nodeId = TaggedId@1.value }
    self.nodeId = nt_TaggedId_1.value;

    // TaggedBool@0(3)[TaggedId@1.END, EOI]
    left = nt_TaggedId_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedBool@0(3)[TaggedId@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool_0 = TaggedBool(input, begin + left, begin + right, 3);
    if (nt_TaggedBool_0 === null) break _ipg_alt;
    if (nt_TaggedBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool_0._ipg_end);
    }
    nt_TaggedBool_0._ipg_end += left;
    nt_TaggedBool_0._ipg_start += left;
    left = nt_TaggedBool_0._ipg_start;
    right = nt_TaggedBool_0._ipg_end;

    // { isUpdate = TaggedBool@0.value }
    self.isUpdate = nt_TaggedBool_0.value;

    // SubBlock@0(4)[TaggedBool@0.END, EOI]
    left = nt_TaggedBool_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@0(4)[TaggedBool@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 4);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@2(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@2(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_2 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_2 === null) break _ipg_alt;
    if (nt_TaggedId_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_2._ipg_end);
    }
    nt_TaggedId_2._ipg_end += left;
    nt_TaggedId_2._ipg_start += left;
    left = nt_TaggedId_2._ipg_start;
    right = nt_TaggedId_2._ipg_end;

    // { parentId = TaggedId@2.value }
    self.parentId = nt_TaggedId_2.value;

    // ExpectEmpty@0[TaggedId@2.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_2._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedId@2.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneInfo",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWID_0;
    let nt_OptionalLWWBool_0;
    let nt_OptionalLWWBool_1;
    let nt_OptionalIntPair_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWID@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LWWID@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWID_0 = LWWID(input, begin + left, begin + right, 1);
    if (nt_LWWID_0 === null) break _ipg_alt;
    if (nt_LWWID_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWID_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWID_0._ipg_end);
    }
    nt_LWWID_0._ipg_end += left;
    nt_LWWID_0._ipg_start += left;
    left = nt_LWWID_0._ipg_start;
    right = nt_LWWID_0._ipg_end;

    // { currentLayer = LWWID@0.value }
    self.currentLayer = nt_LWWID_0.value;

    // OptionalLWWBool@0(2)[LWWID@0.END, EOI]
    left = nt_LWWID_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWBool@0(2)[LWWID@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWBool_0 = OptionalLWWBool(input, begin + left, begin + right, 2);
    if (nt_OptionalLWWBool_0 === null) break _ipg_alt;
    if (nt_OptionalLWWBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWBool_0._ipg_end);
    }
    nt_OptionalLWWBool_0._ipg_end += left;
    nt_OptionalLWWBool_0._ipg_start += left;
    left = nt_OptionalLWWBool_0._ipg_start;
    right = nt_OptionalLWWBool_0._ipg_end;

    // { backgroundVisible = OptionalLWWBool@0.value }
    self.backgroundVisible = nt_OptionalLWWBool_0.value;

    // OptionalLWWBool@1(3)[OptionalLWWBool@0.END, EOI]
    left = nt_OptionalLWWBool_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalLWWBool@1(3)[OptionalLWWBool@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalLWWBool_1 = OptionalLWWBool(input, begin + left, begin + right, 3);
    if (nt_OptionalLWWBool_1 === null) break _ipg_alt;
    if (nt_OptionalLWWBool_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalLWWBool_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalLWWBool_1._ipg_end);
    }
    nt_OptionalLWWBool_1._ipg_end += left;
    nt_OptionalLWWBool_1._ipg_start += left;
    left = nt_OptionalLWWBool_1._ipg_start;
    right = nt_OptionalLWWBool_1._ipg_end;

    // { rootDocumentVisible = OptionalLWWBool@1.value }
    self.rootDocumentVisible = nt_OptionalLWWBool_1.value;

    // OptionalIntPair@0(4)[OptionalLWWBool@1.END, EOI]
    left = nt_OptionalLWWBool_1._ipg_end;
    _ipg_failedTerm = { term: "OptionalIntPair@0(4)[OptionalLWWBool@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalIntPair_0 = OptionalIntPair(input, begin + left, begin + right, 4);
    if (nt_OptionalIntPair_0 === null) break _ipg_alt;
    if (nt_OptionalIntPair_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalIntPair_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalIntPair_0._ipg_end);
    }
    nt_OptionalIntPair_0._ipg_end += left;
    nt_OptionalIntPair_0._ipg_start += left;
    left = nt_OptionalIntPair_0._ipg_start;
    right = nt_OptionalIntPair_0._ipg_end;

    // { paperSize = OptionalIntPair@0.value }
    self.paperSize = nt_OptionalIntPair_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneItemInfo(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneItemInfo",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    let nt_TaggedId_1;
    let nt_TaggedId_2;
    let nt_TaggedId_3;
    let nt_TaggedU32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { parentId = TaggedId@0.value }
    self.parentId = nt_TaggedId_0.value;

    // TaggedId@1(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@1(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_1 = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId_1 === null) break _ipg_alt;
    if (nt_TaggedId_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_1._ipg_end);
    }
    nt_TaggedId_1._ipg_end += left;
    nt_TaggedId_1._ipg_start += left;
    left = nt_TaggedId_1._ipg_start;
    right = nt_TaggedId_1._ipg_end;

    // { itemId = TaggedId@1.value }
    self.itemId = nt_TaggedId_1.value;

    // TaggedId@2(3)[TaggedId@1.END, EOI]
    left = nt_TaggedId_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@2(3)[TaggedId@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_2 = TaggedId(input, begin + left, begin + right, 3);
    if (nt_TaggedId_2 === null) break _ipg_alt;
    if (nt_TaggedId_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_2._ipg_end);
    }
    nt_TaggedId_2._ipg_end += left;
    nt_TaggedId_2._ipg_start += left;
    left = nt_TaggedId_2._ipg_start;
    right = nt_TaggedId_2._ipg_end;

    // { leftId = TaggedId@2.value }
    self.leftId = nt_TaggedId_2.value;

    // TaggedId@3(4)[TaggedId@2.END, EOI]
    left = nt_TaggedId_2._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@3(4)[TaggedId@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_3 = TaggedId(input, begin + left, begin + right, 4);
    if (nt_TaggedId_3 === null) break _ipg_alt;
    if (nt_TaggedId_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_3._ipg_end);
    }
    nt_TaggedId_3._ipg_end += left;
    nt_TaggedId_3._ipg_start += left;
    left = nt_TaggedId_3._ipg_start;
    right = nt_TaggedId_3._ipg_end;

    // { rightId = TaggedId@3.value }
    self.rightId = nt_TaggedId_3.value;

    // TaggedU32@0(5)[TaggedId@3.END, EOI]
    left = nt_TaggedId_3._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@0(5)[TaggedId@3.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, 5);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { deletedLength = TaggedU32@0.value }
    self.deletedLength = nt_TaggedU32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneTombstoneItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneTombstoneItemBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneItemInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo_0 = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo_0 === null) break _ipg_alt;
    if (nt_SceneItemInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo_0._ipg_end);
    }
    nt_SceneItemInfo_0._ipg_end += left;
    nt_SceneItemInfo_0._ipg_start += left;
    left = nt_SceneItemInfo_0._ipg_start;
    right = nt_SceneItemInfo_0._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo@0.this) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo_0));

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneGlyphItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneGlyphItemBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo_0;
    let nt_SceneGlyphItemBlockValue_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneItemInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo_0 = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo_0 === null) break _ipg_alt;
    if (nt_SceneItemInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo_0._ipg_end);
    }
    nt_SceneItemInfo_0._ipg_end += left;
    nt_SceneItemInfo_0._ipg_start += left;
    left = nt_SceneItemInfo_0._ipg_start;
    right = nt_SceneItemInfo_0._ipg_end;

    // SceneGlyphItemBlockValue@0[SceneItemInfo@0.END, EOI]
    left = nt_SceneItemInfo_0._ipg_end;
    _ipg_failedTerm = { term: "SceneGlyphItemBlockValue@0[SceneItemInfo@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGlyphItemBlockValue_0 = SceneGlyphItemBlockValue(input, begin + left, begin + right);
    if (nt_SceneGlyphItemBlockValue_0 === null) break _ipg_alt;
    if (nt_SceneGlyphItemBlockValue_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGlyphItemBlockValue_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGlyphItemBlockValue_0._ipg_end);
    }
    nt_SceneGlyphItemBlockValue_0._ipg_end += left;
    nt_SceneGlyphItemBlockValue_0._ipg_start += left;
    left = nt_SceneGlyphItemBlockValue_0._ipg_start;
    right = nt_SceneGlyphItemBlockValue_0._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo@0.this, SceneGlyphItemBlockValue@0.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo_0), nt_SceneGlyphItemBlockValue_0.value);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneGlyphItemBlockValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneGlyphItemBlockValue",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_OptionalU32_0;
    let nt_OptionalU32_1;
    let nt_TaggedU32_0;
    let nt_String_0;
    let nt_SubBlock_1;
    let nt_VarUInt_0;
    let nt_Rectangle_0;
    let nt_ExpectEmpty_0;
    let nt_ExpectEmpty_1;
    let seq_Rectangle_0; let seq_Rectangle_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(6)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(6)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // { outerEnd = SubBlock@0.END + SubBlock@0.length }
    self.outerEnd = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;

    // "\x01"[SubBlock@0.END, SubBlock@0.END + 1]
    left = nt_SubBlock_0._ipg_end;
    right = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "\"\\x01\"[SubBlock@0.END, SubBlock@0.END + 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // OptionalU32@0(2)[SubBlock@0.END + 1, EOI]
    left = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "OptionalU32@0(2)[SubBlock@0.END + 1, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32_0 = OptionalU32(input, begin + left, begin + right, 2);
    if (nt_OptionalU32_0 === null) break _ipg_alt;
    if (nt_OptionalU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32_0._ipg_end);
    }
    nt_OptionalU32_0._ipg_end += left;
    nt_OptionalU32_0._ipg_start += left;
    left = nt_OptionalU32_0._ipg_start;
    right = nt_OptionalU32_0._ipg_end;

    // { optStart = OptionalU32@0.value }
    self.optStart = nt_OptionalU32_0.value;

    // OptionalU32@1(3)[OptionalU32@0.END, EOI]
    left = nt_OptionalU32_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalU32@1(3)[OptionalU32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32_1 = OptionalU32(input, begin + left, begin + right, 3);
    if (nt_OptionalU32_1 === null) break _ipg_alt;
    if (nt_OptionalU32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32_1._ipg_end);
    }
    nt_OptionalU32_1._ipg_end += left;
    nt_OptionalU32_1._ipg_start += left;
    left = nt_OptionalU32_1._ipg_start;
    right = nt_OptionalU32_1._ipg_end;

    // { optLength = OptionalU32@1.value }
    self.optLength = nt_OptionalU32_1.value;

    // TaggedU32@0(4)[OptionalU32@1.END, EOI]
    left = nt_OptionalU32_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@0(4)[OptionalU32@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, 4);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { colorId = TaggedU32@0.value }
    self.colorId = nt_TaggedU32_0.value;

    // String@0(5)[TaggedU32@0.END, EOI]
    left = nt_TaggedU32_0._ipg_end;
    _ipg_failedTerm = { term: "String@0(5)[TaggedU32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = String(input, begin + left, begin + right, 5);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { text = String@0.value }
    self.text = nt_String_0.value;

    // { start = optStart == getNull() ? 0 : optStart }
    self.start = self.optStart == getNull() ? 0 : self.optStart;

    // { length = optLength == getNull() ? getLength(text) : optLength }
    self.length = self.optLength == getNull() ? getLength(self.text) : self.optLength;

    // SubBlock@1(6)[String@0.END, EOI]
    left = nt_String_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@1(6)[String@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_1 = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock_1 === null) break _ipg_alt;
    if (nt_SubBlock_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_1._ipg_end);
    }
    nt_SubBlock_1._ipg_end += left;
    nt_SubBlock_1._ipg_start += left;
    left = nt_SubBlock_1._ipg_start;
    right = nt_SubBlock_1._ipg_end;

    // VarUInt@0[SubBlock@1.END + 1, SubBlock@1.END + SubBlock@1.length]
    left = nt_SubBlock_1._ipg_end + 1;
    _ipg_failedTerm = { term: "VarUInt@0[SubBlock@1.END + 1, SubBlock@1.END + SubBlock@1.length]", left, right };
    right = nt_SubBlock_1._ipg_end + nt_SubBlock_1.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // for i = 0 to VarUInt@0.value do Rectangle@0[VarUInt@0.END + 32 * i, SubBlock@1.END + SubBlock@1.length]
    _ipg_failedTerm = { term: "for i = 0 to VarUInt@0.value do Rectangle@0[VarUInt@0.END + 32 * i, SubBlock@1.END + SubBlock@1.length]" };
    nt_Rectangle_0 = { _ipg_end: right, _ipg_start: left };
    seq_Rectangle_0_start = 0;
    loopEnd = nt_VarUInt_0.value;
    seq_Rectangle_0 = new Array(Math.max(0, loopEnd - seq_Rectangle_0_start));
    for (let i_i = seq_Rectangle_0_start; i_i < loopEnd; i_i++) {
      const left = nt_VarUInt_0._ipg_end + 32 * i_i;
      const right = nt_SubBlock_1._ipg_end + nt_SubBlock_1.length;
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Rectangle(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Rectangle_0._ipg_end = tmp._ipg_end;
      nt_Rectangle_0._ipg_start = tmp._ipg_start;
      seq_Rectangle_0[i_i - seq_Rectangle_0_start] = tmp;
    }
    left = nt_Rectangle_0._ipg_start;
    right = nt_Rectangle_0._ipg_end;

    // { rectangles = Rectangle@0.these }
    self.rectangles = seq_Rectangle_0.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty@0[Rectangle@0.END, SubBlock@1.END + SubBlock@1.length]
    left = nt_Rectangle_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[Rectangle@0.END, SubBlock@1.END + SubBlock@1.length]", left, right };
    right = nt_SubBlock_1._ipg_end + nt_SubBlock_1.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

    // ExpectEmpty@1[ExpectEmpty@0.END, outerEnd]
    left = nt_ExpectEmpty_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@1[ExpectEmpty@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_1 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_1 === null) break _ipg_alt;
    if (nt_ExpectEmpty_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_1._ipg_end);
    }
    nt_ExpectEmpty_1._ipg_end += left;
    nt_ExpectEmpty_1._ipg_start += left;
    left = nt_ExpectEmpty_1._ipg_start;
    right = nt_ExpectEmpty_1._ipg_end;

    // { value = makeSceneGlyItemBlockValue(start, length, colorId, text, rectangles) }
    self.value = makeSceneGlyItemBlockValue(self.start, self.length, self.colorId, self.text, self.rectangles);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Rectangle(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Rectangle",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float64_0;
    let nt_Float64_1;
    let nt_Float64_2;
    let nt_Float64_3;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Float64@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Float64@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_0 = Float64(input, begin + left, begin + right);
    if (nt_Float64_0 === null) break _ipg_alt;
    if (nt_Float64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_0._ipg_end);
    }
    nt_Float64_0._ipg_end += left;
    nt_Float64_0._ipg_start += left;
    left = nt_Float64_0._ipg_start;
    right = nt_Float64_0._ipg_end;

    // { x = Float64@0.value }
    self.x = nt_Float64_0.value;

    // Float64@1[Float64@0.END, EOI]
    left = nt_Float64_0._ipg_end;
    _ipg_failedTerm = { term: "Float64@1[Float64@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_1 = Float64(input, begin + left, begin + right);
    if (nt_Float64_1 === null) break _ipg_alt;
    if (nt_Float64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_1._ipg_end);
    }
    nt_Float64_1._ipg_end += left;
    nt_Float64_1._ipg_start += left;
    left = nt_Float64_1._ipg_start;
    right = nt_Float64_1._ipg_end;

    // { y = Float64@1.value }
    self.y = nt_Float64_1.value;

    // Float64@2[Float64@1.END, EOI]
    left = nt_Float64_1._ipg_end;
    _ipg_failedTerm = { term: "Float64@2[Float64@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_2 = Float64(input, begin + left, begin + right);
    if (nt_Float64_2 === null) break _ipg_alt;
    if (nt_Float64_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_2._ipg_end);
    }
    nt_Float64_2._ipg_end += left;
    nt_Float64_2._ipg_start += left;
    left = nt_Float64_2._ipg_start;
    right = nt_Float64_2._ipg_end;

    // { width = Float64@2.value }
    self.width = nt_Float64_2.value;

    // Float64@3[Float64@2.END, EOI]
    left = nt_Float64_2._ipg_end;
    _ipg_failedTerm = { term: "Float64@3[Float64@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_3 = Float64(input, begin + left, begin + right);
    if (nt_Float64_3 === null) break _ipg_alt;
    if (nt_Float64_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_3._ipg_end);
    }
    nt_Float64_3._ipg_end += left;
    nt_Float64_3._ipg_start += left;
    left = nt_Float64_3._ipg_start;
    right = nt_Float64_3._ipg_end;

    // { height = Float64@3.value }
    self.height = nt_Float64_3.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneGroupItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneGroupItemBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo_0;
    let nt_SceneGroupItemBlockValue_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneItemInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo_0 = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo_0 === null) break _ipg_alt;
    if (nt_SceneItemInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo_0._ipg_end);
    }
    nt_SceneItemInfo_0._ipg_end += left;
    nt_SceneItemInfo_0._ipg_start += left;
    left = nt_SceneItemInfo_0._ipg_start;
    right = nt_SceneItemInfo_0._ipg_end;

    // SceneGroupItemBlockValue@0[SceneItemInfo@0.END, EOI]
    left = nt_SceneItemInfo_0._ipg_end;
    _ipg_failedTerm = { term: "SceneGroupItemBlockValue@0[SceneItemInfo@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneGroupItemBlockValue_0 = SceneGroupItemBlockValue(input, begin + left, begin + right);
    if (nt_SceneGroupItemBlockValue_0 === null) break _ipg_alt;
    if (nt_SceneGroupItemBlockValue_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneGroupItemBlockValue_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneGroupItemBlockValue_0._ipg_end);
    }
    nt_SceneGroupItemBlockValue_0._ipg_end += left;
    nt_SceneGroupItemBlockValue_0._ipg_start += left;
    left = nt_SceneGroupItemBlockValue_0._ipg_start;
    right = nt_SceneGroupItemBlockValue_0._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo@0.this, SceneGroupItemBlockValue@0.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo_0), nt_SceneGroupItemBlockValue_0.value);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneGroupItemBlockValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneGroupItemBlockValue",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(6)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(6)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // "\x02"[SubBlock@0.END, SubBlock@0.END + 1]
    left = nt_SubBlock_0._ipg_end;
    right = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "\"\\x02\"[SubBlock@0.END, SubBlock@0.END + 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x02")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // TaggedId@0(2)[SubBlock@0.END + 1, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "TaggedId@0(2)[SubBlock@0.END + 1, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { value = TaggedId@0.value }
    self.value = nt_TaggedId_0.value;

    // ExpectEmpty@0[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneLineItemBlock(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneLineItemBlock",
    args: [a_version],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo_0;
    let nt_SceneLineItemBlockValue_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneItemInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo_0 = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo_0 === null) break _ipg_alt;
    if (nt_SceneItemInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo_0._ipg_end);
    }
    nt_SceneItemInfo_0._ipg_end += left;
    nt_SceneItemInfo_0._ipg_start += left;
    left = nt_SceneItemInfo_0._ipg_start;
    right = nt_SceneItemInfo_0._ipg_end;

    // SceneLineItemBlockValue@0(version)[SceneItemInfo@0.END, EOI]
    left = nt_SceneItemInfo_0._ipg_end;
    _ipg_failedTerm = { term: "SceneLineItemBlockValue@0(version)[SceneItemInfo@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneLineItemBlockValue_0 = SceneLineItemBlockValue(input, begin + left, begin + right, a_version);
    if (nt_SceneLineItemBlockValue_0 === null) break _ipg_alt;
    if (nt_SceneLineItemBlockValue_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneLineItemBlockValue_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneLineItemBlockValue_0._ipg_end);
    }
    nt_SceneLineItemBlockValue_0._ipg_end += left;
    nt_SceneLineItemBlockValue_0._ipg_start += left;
    left = nt_SceneLineItemBlockValue_0._ipg_start;
    right = nt_SceneLineItemBlockValue_0._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo@0.this, SceneLineItemBlockValue@0.value) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo_0), nt_SceneLineItemBlockValue_0.value);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SceneLineItemBlockValue(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneLineItemBlockValue",
    args: [a_version],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedU32_0;
    let nt_TaggedU32_1;
    let nt_TaggedFloat64_0;
    let nt_TaggedFloat32_0;
    let nt_SubBlock_1;
    let nt_Point_0;
    let nt_ExpectEmpty_0;
    let nt_TaggedId_0;
    let nt_OptionalTaggedId_0;
    let nt_ExpectEmpty_1;
    let seq_Point_0; let seq_Point_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(6)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(6)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 6);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // { outerEnd = SubBlock@0.END + SubBlock@0.length }
    self.outerEnd = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;

    // "\x03"[SubBlock@0.END, SubBlock@0.END + 1]
    left = nt_SubBlock_0._ipg_end;
    right = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "\"\\x03\"[SubBlock@0.END, SubBlock@0.END + 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x03")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // TaggedU32@0(1)[SubBlock@0.END + 1, outerEnd]
    left = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "TaggedU32@0(1)[SubBlock@0.END + 1, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, 1);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { toolId = TaggedU32@0.value }
    self.toolId = nt_TaggedU32_0.value;

    // TaggedU32@1(2)[TaggedU32@0.END, outerEnd]
    left = nt_TaggedU32_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@1(2)[TaggedU32@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_1 = TaggedU32(input, begin + left, begin + right, 2);
    if (nt_TaggedU32_1 === null) break _ipg_alt;
    if (nt_TaggedU32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_1._ipg_end);
    }
    nt_TaggedU32_1._ipg_end += left;
    nt_TaggedU32_1._ipg_start += left;
    left = nt_TaggedU32_1._ipg_start;
    right = nt_TaggedU32_1._ipg_end;

    // { colorId = TaggedU32@1.value }
    self.colorId = nt_TaggedU32_1.value;

    // TaggedFloat64@0(3)[TaggedU32@1.END, outerEnd]
    left = nt_TaggedU32_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedFloat64@0(3)[TaggedU32@1.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat64_0 = TaggedFloat64(input, begin + left, begin + right, 3);
    if (nt_TaggedFloat64_0 === null) break _ipg_alt;
    if (nt_TaggedFloat64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat64_0._ipg_end);
    }
    nt_TaggedFloat64_0._ipg_end += left;
    nt_TaggedFloat64_0._ipg_start += left;
    left = nt_TaggedFloat64_0._ipg_start;
    right = nt_TaggedFloat64_0._ipg_end;

    // { thicknessScale = TaggedFloat64@0.value }
    self.thicknessScale = nt_TaggedFloat64_0.value;

    // TaggedFloat32@0(4)[TaggedFloat64@0.END, outerEnd]
    left = nt_TaggedFloat64_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedFloat32@0(4)[TaggedFloat64@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32_0 = TaggedFloat32(input, begin + left, begin + right, 4);
    if (nt_TaggedFloat32_0 === null) break _ipg_alt;
    if (nt_TaggedFloat32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32_0._ipg_end);
    }
    nt_TaggedFloat32_0._ipg_end += left;
    nt_TaggedFloat32_0._ipg_start += left;
    left = nt_TaggedFloat32_0._ipg_start;
    right = nt_TaggedFloat32_0._ipg_end;

    // { startLength = TaggedFloat32@0.value }
    self.startLength = nt_TaggedFloat32_0.value;

    // SubBlock@1(5)[TaggedFloat32@0.END, outerEnd]
    left = nt_TaggedFloat32_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@1(5)[TaggedFloat32@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_1 = SubBlock(input, begin + left, begin + right, 5);
    if (nt_SubBlock_1 === null) break _ipg_alt;
    if (nt_SubBlock_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_1._ipg_end);
    }
    nt_SubBlock_1._ipg_end += left;
    nt_SubBlock_1._ipg_start += left;
    left = nt_SubBlock_1._ipg_start;
    right = nt_SubBlock_1._ipg_end;

    // { pointSize = pointSerializedSize(version) }
    self.pointSize = pointSerializedSize(a_version);

    // ?[ check(SubBlock@1.length % pointSize == 0, "Point size does not divide subblock size") ]
    _ipg_failedTerm = { term: "?[ check(SubBlock@1.length % pointSize == 0, \"Point size does not divide subblock size\") ]" };
    if (!check(nt_SubBlock_1.length % self.pointSize == 0, "Point size does not divide subblock size")) break _ipg_alt;

    // for i = 0 to SubBlock@1.length / pointSize do Point@0(version)[SubBlock@1.END + pointSize * i, SubBlock@1.END + pointSize * (i + 1)]
    _ipg_failedTerm = { term: "for i = 0 to SubBlock@1.length / pointSize do Point@0(version)[SubBlock@1.END + pointSize * i, SubBlock@1.END + pointSize * (i + 1)]" };
    nt_Point_0 = { _ipg_end: right, _ipg_start: left };
    seq_Point_0_start = 0;
    loopEnd = nt_SubBlock_1.length / self.pointSize;
    seq_Point_0 = new Array(Math.max(0, loopEnd - seq_Point_0_start));
    for (let i_i = seq_Point_0_start; i_i < loopEnd; i_i++) {
      const left = nt_SubBlock_1._ipg_end + self.pointSize * i_i;
      const right = nt_SubBlock_1._ipg_end + self.pointSize * (i_i + 1);
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Point(input, begin + left, begin + right, a_version);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Point_0._ipg_end = tmp._ipg_end;
      nt_Point_0._ipg_start = tmp._ipg_start;
      seq_Point_0[i_i - seq_Point_0_start] = tmp;
    }
    left = nt_Point_0._ipg_start;
    right = nt_Point_0._ipg_end;

    // { points = Point@0.these }
    self.points = seq_Point_0.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty@0[Point@0.END, SubBlock@1.END + SubBlock@1.length]
    left = nt_Point_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[Point@0.END, SubBlock@1.END + SubBlock@1.length]", left, right };
    right = nt_SubBlock_1._ipg_end + nt_SubBlock_1.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

    // TaggedId@0(6)[ExpectEmpty@0.END, outerEnd]
    left = nt_ExpectEmpty_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(6)[ExpectEmpty@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 6);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { timestamp = TaggedId@0.value }
    self.timestamp = nt_TaggedId_0.value;

    // OptionalTaggedId@0(7)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalTaggedId@0(7)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalTaggedId_0 = OptionalTaggedId(input, begin + left, begin + right, 7);
    if (nt_OptionalTaggedId_0 === null) break _ipg_alt;
    if (nt_OptionalTaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalTaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalTaggedId_0._ipg_end);
    }
    nt_OptionalTaggedId_0._ipg_end += left;
    nt_OptionalTaggedId_0._ipg_start += left;
    left = nt_OptionalTaggedId_0._ipg_start;
    right = nt_OptionalTaggedId_0._ipg_end;

    // { moveId = OptionalTaggedId@0.value }
    self.moveId = nt_OptionalTaggedId_0.value;

    // ExpectEmpty@1[OptionalTaggedId@0.END, outerEnd]
    left = nt_OptionalTaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@1[OptionalTaggedId@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_1 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_1 === null) break _ipg_alt;
    if (nt_ExpectEmpty_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_1._ipg_end);
    }
    nt_ExpectEmpty_1._ipg_end += left;
    nt_ExpectEmpty_1._ipg_start += left;
    left = nt_ExpectEmpty_1._ipg_start;
    right = nt_ExpectEmpty_1._ipg_end;

    // { value = makeSceneLineItemBlockValue(toolId, colorId, thicknessScale, startLength, points, moveId) }
    self.value = makeSceneLineItemBlockValue(self.toolId, self.colorId, self.thicknessScale, self.startLength, self.points, self.moveId);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Point(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Point",
    args: [a_version],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float32_0;
    let nt_Float32_1;
    let nt_PointVersions_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Float32@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Float32@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_0 = Float32(input, begin + left, begin + right);
    if (nt_Float32_0 === null) break _ipg_alt;
    if (nt_Float32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_0._ipg_end);
    }
    nt_Float32_0._ipg_end += left;
    nt_Float32_0._ipg_start += left;
    left = nt_Float32_0._ipg_start;
    right = nt_Float32_0._ipg_end;

    // { x = Float32@0.value }
    self.x = nt_Float32_0.value;

    // Float32@1[Float32@0.END, EOI]
    left = nt_Float32_0._ipg_end;
    _ipg_failedTerm = { term: "Float32@1[Float32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_1 = Float32(input, begin + left, begin + right);
    if (nt_Float32_1 === null) break _ipg_alt;
    if (nt_Float32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_1._ipg_end);
    }
    nt_Float32_1._ipg_end += left;
    nt_Float32_1._ipg_start += left;
    left = nt_Float32_1._ipg_start;
    right = nt_Float32_1._ipg_end;

    // { y = Float32@1.value }
    self.y = nt_Float32_1.value;

    // PointVersions@0(version)[Float32@1.END, EOI]
    left = nt_Float32_1._ipg_end;
    _ipg_failedTerm = { term: "PointVersions@0(version)[Float32@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PointVersions_0 = PointVersions(input, begin + left, begin + right, a_version);
    if (nt_PointVersions_0 === null) break _ipg_alt;
    if (nt_PointVersions_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PointVersions_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PointVersions_0._ipg_end);
    }
    nt_PointVersions_0._ipg_end += left;
    nt_PointVersions_0._ipg_start += left;
    left = nt_PointVersions_0._ipg_start;
    right = nt_PointVersions_0._ipg_end;

    // { speed = PointVersions@0.speed }
    self.speed = nt_PointVersions_0.speed;

    // { width = PointVersions@0.width }
    self.width = nt_PointVersions_0.width;

    // { direction = PointVersions@0.direction }
    self.direction = nt_PointVersions_0.direction;

    // { pressure = PointVersions@0.pressure }
    self.pressure = nt_PointVersions_0.pressure;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function PointVersions(input, begin = 0, end = input.length, a_version) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "PointVersions",
    args: [a_version],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Float32_0;
    let nt_Float32_1;
    let nt_Float32_2;
    let nt_Float32_3;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ version == 1 ]
    _ipg_failedTerm = { term: "?[ version == 1 ]" };
    if (!(a_version == 1)) break _ipg_alt;

    // Float32@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Float32@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_0 = Float32(input, begin + left, begin + right);
    if (nt_Float32_0 === null) break _ipg_alt;
    if (nt_Float32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_0._ipg_end);
    }
    nt_Float32_0._ipg_end += left;
    nt_Float32_0._ipg_start += left;
    left = nt_Float32_0._ipg_start;
    right = nt_Float32_0._ipg_end;

    // { speed = 4 * Float32@0.value }
    self.speed = 4 * nt_Float32_0.value;

    // Float32@1[Float32@0.END, EOI]
    left = nt_Float32_0._ipg_end;
    _ipg_failedTerm = { term: "Float32@1[Float32@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_1 = Float32(input, begin + left, begin + right);
    if (nt_Float32_1 === null) break _ipg_alt;
    if (nt_Float32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_1._ipg_end);
    }
    nt_Float32_1._ipg_end += left;
    nt_Float32_1._ipg_start += left;
    left = nt_Float32_1._ipg_start;
    right = nt_Float32_1._ipg_end;

    // { direction = 255 * Float32@1.value / getTwoPi() }
    self.direction = 255 * nt_Float32_1.value / getTwoPi();

    // Float32@2[Float32@1.END, EOI]
    left = nt_Float32_1._ipg_end;
    _ipg_failedTerm = { term: "Float32@2[Float32@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_2 = Float32(input, begin + left, begin + right);
    if (nt_Float32_2 === null) break _ipg_alt;
    if (nt_Float32_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_2._ipg_end);
    }
    nt_Float32_2._ipg_end += left;
    nt_Float32_2._ipg_start += left;
    left = nt_Float32_2._ipg_start;
    right = nt_Float32_2._ipg_end;

    // { width = int(round(4 * Float32@2.value)) }
    self.width = int(round(4 * nt_Float32_2.value));

    // Float32@3[Float32@2.END, EOI]
    left = nt_Float32_2._ipg_end;
    _ipg_failedTerm = { term: "Float32@3[Float32@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_3 = Float32(input, begin + left, begin + right);
    if (nt_Float32_3 === null) break _ipg_alt;
    if (nt_Float32_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_3._ipg_end);
    }
    nt_Float32_3._ipg_end += left;
    nt_Float32_3._ipg_start += left;
    left = nt_Float32_3._ipg_start;
    right = nt_Float32_3._ipg_end;

    // { pressure = 255 * Float32@3.value }
    self.pressure = 255 * nt_Float32_3.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16_0;
    let nt_U16_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ version == 2 ]
    _ipg_failedTerm = { term: "?[ version == 2 ]" };
    if (!(a_version == 2)) break _ipg_alt;

    // U16@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "U16@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_0 = U16(input, begin + left, begin + right);
    if (nt_U16_0 === null) break _ipg_alt;
    if (nt_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_0._ipg_end);
    }
    nt_U16_0._ipg_end += left;
    nt_U16_0._ipg_start += left;
    left = nt_U16_0._ipg_start;
    right = nt_U16_0._ipg_end;

    // { speed = U16@0.value }
    self.speed = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
    _ipg_failedTerm = { term: "U16@1[U16@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_1 = U16(input, begin + left, begin + right);
    if (nt_U16_1 === null) break _ipg_alt;
    if (nt_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_1._ipg_end);
    }
    nt_U16_1._ipg_end += left;
    nt_U16_1._ipg_start += left;
    left = nt_U16_1._ipg_start;
    right = nt_U16_1._ipg_end;

    // { width = U16@1.value }
    self.width = nt_U16_1.value;

    // { direction = .[U16@1.END] }
    left = nt_U16_1._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ direction = .[U16@1.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.direction = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { pressure = .[U16@1.END + 1] }
    left = nt_U16_1._ipg_end + 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ pressure = .[U16@1.END + 1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.pressure = input[begin + left];
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

function SceneTextItemBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SceneTextItemBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SceneItemInfo_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SceneItemInfo@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SceneItemInfo@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SceneItemInfo_0 = SceneItemInfo(input, begin + left, begin + right);
    if (nt_SceneItemInfo_0 === null) break _ipg_alt;
    if (nt_SceneItemInfo_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SceneItemInfo_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SceneItemInfo_0._ipg_end);
    }
    nt_SceneItemInfo_0._ipg_end += left;
    nt_SceneItemInfo_0._ipg_start += left;
    left = nt_SceneItemInfo_0._ipg_start;
    right = nt_SceneItemInfo_0._ipg_end;

    // { item = makeCrdtSequenceItem(SceneItemInfo@0.this) }
    self.item = makeCrdtSequenceItem((({_ipg_start,_ipg_end,...o}) => o)(nt_SceneItemInfo_0));

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function RootTextBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "RootTextBlock",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    let nt_SubBlock_0;
    let nt_SubBlock_1;
    let nt_SubBlock_2;
    let nt_VarUInt_0;
    let nt_TextItem_0;
    let nt_ExpectEmpty_0;
    let nt_ExpectEmpty_1;
    let nt_SubBlock_3;
    let nt_SubBlock_4;
    let nt_VarUInt_1;
    let nt_TextFormat_0;
    let nt_ExpectEmpty_2;
    let nt_ExpectEmpty_3;
    let nt_ExpectEmpty_4;
    let nt_SubBlock_5;
    let nt_Float64_0;
    let nt_Float64_1;
    let nt_ExpectEmpty_5;
    let nt_TaggedFloat32_0;
    let seq_TextItem_0; let seq_TextItem_0_start = 0;
    let seq_TextFormat_0; let seq_TextFormat_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { blockId = TaggedId@0.value }
    self.blockId = nt_TaggedId_0.value;

    // ?[ checkRootTextBlockId(blockId) ]
    _ipg_failedTerm = { term: "?[ checkRootTextBlockId(blockId) ]" };
    if (!checkRootTextBlockId(self.blockId)) break _ipg_alt;

    // SubBlock@0(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@0(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // { outerEnd = SubBlock@0.END + SubBlock@0.length }
    self.outerEnd = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;

    // SubBlock@1(1)[SubBlock@0.END, outerEnd]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@1(1)[SubBlock@0.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_1 = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock_1 === null) break _ipg_alt;
    if (nt_SubBlock_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_1._ipg_end);
    }
    nt_SubBlock_1._ipg_end += left;
    nt_SubBlock_1._ipg_start += left;
    left = nt_SubBlock_1._ipg_start;
    right = nt_SubBlock_1._ipg_end;

    // { innerEnd1 = SubBlock@1.END + SubBlock@1.length }
    self.innerEnd1 = nt_SubBlock_1._ipg_end + nt_SubBlock_1.length;

    // SubBlock@2(1)[SubBlock@1.END, innerEnd1]
    left = nt_SubBlock_1._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@2(1)[SubBlock@1.END, innerEnd1]", left, right };
    right = self.innerEnd1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_2 = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock_2 === null) break _ipg_alt;
    if (nt_SubBlock_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_2._ipg_end);
    }
    nt_SubBlock_2._ipg_end += left;
    nt_SubBlock_2._ipg_start += left;
    left = nt_SubBlock_2._ipg_start;
    right = nt_SubBlock_2._ipg_end;

    // VarUInt@0[SubBlock@2.END, EOI]
    left = nt_SubBlock_2._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@0[SubBlock@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // for i = 0 to VarUInt@0.value do TextItem@0[TextItem@0.END, SubBlock@2.END + SubBlock@2.length]
    _ipg_failedTerm = { term: "for i = 0 to VarUInt@0.value do TextItem@0[TextItem@0.END, SubBlock@2.END + SubBlock@2.length]" };
    nt_TextItem_0 = { _ipg_end: right, _ipg_start: left };
    seq_TextItem_0_start = 0;
    loopEnd = nt_VarUInt_0.value;
    seq_TextItem_0 = new Array(Math.max(0, loopEnd - seq_TextItem_0_start));
    for (let i_i = seq_TextItem_0_start; i_i < loopEnd; i_i++) {
      const left = nt_TextItem_0._ipg_end;
      const right = nt_SubBlock_2._ipg_end + nt_SubBlock_2.length;
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = TextItem(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_TextItem_0._ipg_end = tmp._ipg_end;
      nt_TextItem_0._ipg_start = tmp._ipg_start;
      seq_TextItem_0[i_i - seq_TextItem_0_start] = tmp;
    }
    left = nt_TextItem_0._ipg_start;
    right = nt_TextItem_0._ipg_end;

    // { textItems = TextItem@0.these }
    self.textItems = seq_TextItem_0.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty@0[TextItem@0.END, SubBlock@2.END + SubBlock@2.length]
    left = nt_TextItem_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TextItem@0.END, SubBlock@2.END + SubBlock@2.length]", left, right };
    right = nt_SubBlock_2._ipg_end + nt_SubBlock_2.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

    // ExpectEmpty@1[ExpectEmpty@0.END, innerEnd1]
    left = nt_ExpectEmpty_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@1[ExpectEmpty@0.END, innerEnd1]", left, right };
    right = self.innerEnd1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_1 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_1 === null) break _ipg_alt;
    if (nt_ExpectEmpty_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_1._ipg_end);
    }
    nt_ExpectEmpty_1._ipg_end += left;
    nt_ExpectEmpty_1._ipg_start += left;
    left = nt_ExpectEmpty_1._ipg_start;
    right = nt_ExpectEmpty_1._ipg_end;

    // SubBlock@3(2)[ExpectEmpty@1.END, outerEnd]
    left = nt_ExpectEmpty_1._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@3(2)[ExpectEmpty@1.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_3 = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock_3 === null) break _ipg_alt;
    if (nt_SubBlock_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_3._ipg_end);
    }
    nt_SubBlock_3._ipg_end += left;
    nt_SubBlock_3._ipg_start += left;
    left = nt_SubBlock_3._ipg_start;
    right = nt_SubBlock_3._ipg_end;

    // { innerEnd2 = SubBlock@3.END + SubBlock@3.length }
    self.innerEnd2 = nt_SubBlock_3._ipg_end + nt_SubBlock_3.length;

    // SubBlock@4(1)[SubBlock@3.END, innerEnd2]
    left = nt_SubBlock_3._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@4(1)[SubBlock@3.END, innerEnd2]", left, right };
    right = self.innerEnd2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_4 = SubBlock(input, begin + left, begin + right, 1);
    if (nt_SubBlock_4 === null) break _ipg_alt;
    if (nt_SubBlock_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_4._ipg_end);
    }
    nt_SubBlock_4._ipg_end += left;
    nt_SubBlock_4._ipg_start += left;
    left = nt_SubBlock_4._ipg_start;
    right = nt_SubBlock_4._ipg_end;

    // VarUInt@1[SubBlock@4.END, EOI]
    left = nt_SubBlock_4._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@1[SubBlock@4.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_1 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_1 === null) break _ipg_alt;
    if (nt_VarUInt_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_1._ipg_end);
    }
    nt_VarUInt_1._ipg_end += left;
    nt_VarUInt_1._ipg_start += left;
    left = nt_VarUInt_1._ipg_start;
    right = nt_VarUInt_1._ipg_end;

    // for i = 0 to VarUInt@1.value do TextFormat@0[TextFormat@0.END, SubBlock@4.END + SubBlock@4.length]
    _ipg_failedTerm = { term: "for i = 0 to VarUInt@1.value do TextFormat@0[TextFormat@0.END, SubBlock@4.END + SubBlock@4.length]" };
    nt_TextFormat_0 = { _ipg_end: right, _ipg_start: left };
    seq_TextFormat_0_start = 0;
    loopEnd = nt_VarUInt_1.value;
    seq_TextFormat_0 = new Array(Math.max(0, loopEnd - seq_TextFormat_0_start));
    for (let i_i = seq_TextFormat_0_start; i_i < loopEnd; i_i++) {
      const left = nt_TextFormat_0._ipg_end;
      const right = nt_SubBlock_4._ipg_end + nt_SubBlock_4.length;
    _ipg_failedTerm.left = left; _ipg_failedTerm.right = right;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = TextFormat(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_TextFormat_0._ipg_end = tmp._ipg_end;
      nt_TextFormat_0._ipg_start = tmp._ipg_start;
      seq_TextFormat_0[i_i - seq_TextFormat_0_start] = tmp;
    }
    left = nt_TextFormat_0._ipg_start;
    right = nt_TextFormat_0._ipg_end;

    // { textFormats = TextFormat@0.these }
    self.textFormats = seq_TextFormat_0.map(({_ipg_start,_ipg_end,...o}) => o);

    // ExpectEmpty@2[TextFormat@0.END, SubBlock@4.END + SubBlock@4.length]
    left = nt_TextFormat_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@2[TextFormat@0.END, SubBlock@4.END + SubBlock@4.length]", left, right };
    right = nt_SubBlock_4._ipg_end + nt_SubBlock_4.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_2 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_2 === null) break _ipg_alt;
    if (nt_ExpectEmpty_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_2._ipg_end);
    }
    nt_ExpectEmpty_2._ipg_end += left;
    nt_ExpectEmpty_2._ipg_start += left;
    left = nt_ExpectEmpty_2._ipg_start;
    right = nt_ExpectEmpty_2._ipg_end;

    // ExpectEmpty@3[ExpectEmpty@2.END, innerEnd2]
    left = nt_ExpectEmpty_2._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@3[ExpectEmpty@2.END, innerEnd2]", left, right };
    right = self.innerEnd2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_3 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_3 === null) break _ipg_alt;
    if (nt_ExpectEmpty_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_3._ipg_end);
    }
    nt_ExpectEmpty_3._ipg_end += left;
    nt_ExpectEmpty_3._ipg_start += left;
    left = nt_ExpectEmpty_3._ipg_start;
    right = nt_ExpectEmpty_3._ipg_end;

    // ExpectEmpty@4[ExpectEmpty@3.END, outerEnd]
    left = nt_ExpectEmpty_3._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@4[ExpectEmpty@3.END, outerEnd]", left, right };
    right = self.outerEnd;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_4 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_4 === null) break _ipg_alt;
    if (nt_ExpectEmpty_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_4._ipg_end);
    }
    nt_ExpectEmpty_4._ipg_end += left;
    nt_ExpectEmpty_4._ipg_start += left;
    left = nt_ExpectEmpty_4._ipg_start;
    right = nt_ExpectEmpty_4._ipg_end;

    // SubBlock@5(3)[ExpectEmpty@4.END, EOI]
    left = nt_ExpectEmpty_4._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@5(3)[ExpectEmpty@4.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_5 = SubBlock(input, begin + left, begin + right, 3);
    if (nt_SubBlock_5 === null) break _ipg_alt;
    if (nt_SubBlock_5._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_5._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_5._ipg_end);
    }
    nt_SubBlock_5._ipg_end += left;
    nt_SubBlock_5._ipg_start += left;
    left = nt_SubBlock_5._ipg_start;
    right = nt_SubBlock_5._ipg_end;

    // Float64@0[SubBlock@5.END, SubBlock@5.END + SubBlock@5.length]
    left = nt_SubBlock_5._ipg_end;
    _ipg_failedTerm = { term: "Float64@0[SubBlock@5.END, SubBlock@5.END + SubBlock@5.length]", left, right };
    right = nt_SubBlock_5._ipg_end + nt_SubBlock_5.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_0 = Float64(input, begin + left, begin + right);
    if (nt_Float64_0 === null) break _ipg_alt;
    if (nt_Float64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_0._ipg_end);
    }
    nt_Float64_0._ipg_end += left;
    nt_Float64_0._ipg_start += left;
    left = nt_Float64_0._ipg_start;
    right = nt_Float64_0._ipg_end;

    // { xPosition = Float64@0.value }
    self.xPosition = nt_Float64_0.value;

    // Float64@1[Float64@0.END, SubBlock@5.END + SubBlock@5.length]
    left = nt_Float64_0._ipg_end;
    _ipg_failedTerm = { term: "Float64@1[Float64@0.END, SubBlock@5.END + SubBlock@5.length]", left, right };
    right = nt_SubBlock_5._ipg_end + nt_SubBlock_5.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_1 = Float64(input, begin + left, begin + right);
    if (nt_Float64_1 === null) break _ipg_alt;
    if (nt_Float64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_1._ipg_end);
    }
    nt_Float64_1._ipg_end += left;
    nt_Float64_1._ipg_start += left;
    left = nt_Float64_1._ipg_start;
    right = nt_Float64_1._ipg_end;

    // { yPosition = Float64@1.value }
    self.yPosition = nt_Float64_1.value;

    // ExpectEmpty@5[ExpectEmpty@4.END, SubBlock@5.END + SubBlock@5.length]
    left = nt_ExpectEmpty_4._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@5[ExpectEmpty@4.END, SubBlock@5.END + SubBlock@5.length]", left, right };
    right = nt_SubBlock_5._ipg_end + nt_SubBlock_5.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_5 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_5 === null) break _ipg_alt;
    if (nt_ExpectEmpty_5._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_5._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_5._ipg_end);
    }
    nt_ExpectEmpty_5._ipg_end += left;
    nt_ExpectEmpty_5._ipg_start += left;
    left = nt_ExpectEmpty_5._ipg_start;
    right = nt_ExpectEmpty_5._ipg_end;

    // TaggedFloat32@0(4)[ExpectEmpty@5.END, EOI]
    left = nt_ExpectEmpty_5._ipg_end;
    _ipg_failedTerm = { term: "TaggedFloat32@0(4)[ExpectEmpty@5.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32_0 = TaggedFloat32(input, begin + left, begin + right, 4);
    if (nt_TaggedFloat32_0 === null) break _ipg_alt;
    if (nt_TaggedFloat32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32_0._ipg_end);
    }
    nt_TaggedFloat32_0._ipg_end += left;
    nt_TaggedFloat32_0._ipg_start += left;
    left = nt_TaggedFloat32_0._ipg_start;
    right = nt_TaggedFloat32_0._ipg_end;

    // { width = TaggedFloat32@0.value }
    self.width = nt_TaggedFloat32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TextItem(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TextItem",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_TaggedId_1;
    let nt_TaggedId_2;
    let nt_TaggedU32_0;
    let nt_TextItemValue_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(0)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(0)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 0);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(2)[SubBlock@0.END, EOI]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(2)[SubBlock@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { itemId = TaggedId@0.value }
    self.itemId = nt_TaggedId_0.value;

    // TaggedId@1(3)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@1(3)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_1 = TaggedId(input, begin + left, begin + right, 3);
    if (nt_TaggedId_1 === null) break _ipg_alt;
    if (nt_TaggedId_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_1._ipg_end);
    }
    nt_TaggedId_1._ipg_end += left;
    nt_TaggedId_1._ipg_start += left;
    left = nt_TaggedId_1._ipg_start;
    right = nt_TaggedId_1._ipg_end;

    // { leftId = TaggedId@1.value }
    self.leftId = nt_TaggedId_1.value;

    // TaggedId@2(4)[TaggedId@1.END, EOI]
    left = nt_TaggedId_1._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@2(4)[TaggedId@1.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_2 = TaggedId(input, begin + left, begin + right, 4);
    if (nt_TaggedId_2 === null) break _ipg_alt;
    if (nt_TaggedId_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_2._ipg_end);
    }
    nt_TaggedId_2._ipg_end += left;
    nt_TaggedId_2._ipg_start += left;
    left = nt_TaggedId_2._ipg_start;
    right = nt_TaggedId_2._ipg_end;

    // { rightId = TaggedId@2.value }
    self.rightId = nt_TaggedId_2.value;

    // TaggedU32@0(5)[TaggedId@2.END, EOI]
    left = nt_TaggedId_2._ipg_end;
    _ipg_failedTerm = { term: "TaggedU32@0(5)[TaggedId@2.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, 5);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { deletedLength = TaggedU32@0.value }
    self.deletedLength = nt_TaggedU32_0.value;

    // TextItemValue@0[TaggedU32@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedU32_0._ipg_end;
    _ipg_failedTerm = { term: "TextItemValue@0[TaggedU32@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TextItemValue_0 = TextItemValue(input, begin + left, begin + right);
    if (nt_TextItemValue_0 === null) break _ipg_alt;
    if (nt_TextItemValue_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TextItemValue_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TextItemValue_0._ipg_end);
    }
    nt_TextItemValue_0._ipg_end += left;
    nt_TextItemValue_0._ipg_start += left;
    left = nt_TextItemValue_0._ipg_start;
    right = nt_TextItemValue_0._ipg_end;

    // { value = TextItemValue@0.value }
    self.value = nt_TextItemValue_0.value;

    // ExpectEmpty@0[TextItemValue@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TextItemValue_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TextItemValue@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TextItemValue(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TextItemValue",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_StringWithFormat_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // StringWithFormat@0(6)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "StringWithFormat@0(6)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_StringWithFormat_0 = StringWithFormat(input, begin + left, begin + right, 6);
    if (nt_StringWithFormat_0 === null) break _ipg_alt;
    if (nt_StringWithFormat_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_StringWithFormat_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_StringWithFormat_0._ipg_end);
    }
    nt_StringWithFormat_0._ipg_end += left;
    nt_StringWithFormat_0._ipg_start += left;
    left = nt_StringWithFormat_0._ipg_start;
    right = nt_StringWithFormat_0._ipg_end;

    // { value = processTextItemValue(StringWithFormat@0.value) }
    self.value = processTextItemValue(nt_StringWithFormat_0.value);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = "" }
    self.value = "";

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function StringWithFormat(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "StringWithFormat",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_VarUInt_0;
    let nt_Bool_0;
    let nt_Bytes_0;
    let nt_OptionalU32_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // Bool@0[VarUInt@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_VarUInt_0._ipg_end;
    _ipg_failedTerm = { term: "Bool@0[VarUInt@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool_0 = Bool(input, begin + left, begin + right);
    if (nt_Bool_0 === null) break _ipg_alt;
    if (nt_Bool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool_0._ipg_end);
    }
    nt_Bool_0._ipg_end += left;
    nt_Bool_0._ipg_start += left;
    left = nt_Bool_0._ipg_start;
    right = nt_Bool_0._ipg_end;

    // ?[ check(Bool@0.value, "StringWithFormat flag unset") ]
    _ipg_failedTerm = { term: "?[ check(Bool@0.value, \"StringWithFormat flag unset\") ]" };
    if (!check(nt_Bool_0.value, "StringWithFormat flag unset")) break _ipg_alt;

    // ?[ check(Bool@0.END + VarUInt@0.value <= SubBlock@0.END + SubBlock@0.length, "StringWithFormat: Overfull block") ]
    _ipg_failedTerm = { term: "?[ check(Bool@0.END + VarUInt@0.value <= SubBlock@0.END + SubBlock@0.length, \"StringWithFormat: Overfull block\") ]" };
    if (!check(nt_Bool_0._ipg_end + nt_VarUInt_0.value <= nt_SubBlock_0._ipg_end + nt_SubBlock_0.length, "StringWithFormat: Overfull block")) break _ipg_alt;

    // Bytes@0[Bool@0.END, Bool@0.END + VarUInt@0.value]
    left = nt_Bool_0._ipg_end;
    _ipg_failedTerm = { term: "Bytes@0[Bool@0.END, Bool@0.END + VarUInt@0.value]", left, right };
    right = nt_Bool_0._ipg_end + nt_VarUInt_0.value;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes_0 = Bytes(input, begin + left, begin + right);
    if (nt_Bytes_0 === null) break _ipg_alt;
    if (nt_Bytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes_0._ipg_end);
    }
    nt_Bytes_0._ipg_end += left;
    nt_Bytes_0._ipg_start += left;
    left = nt_Bytes_0._ipg_start;
    right = nt_Bytes_0._ipg_end;

    // OptionalU32@0(2)[Bytes@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_Bytes_0._ipg_end;
    _ipg_failedTerm = { term: "OptionalU32@0(2)[Bytes@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OptionalU32_0 = OptionalU32(input, begin + left, begin + right, 2);
    if (nt_OptionalU32_0 === null) break _ipg_alt;
    if (nt_OptionalU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OptionalU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OptionalU32_0._ipg_end);
    }
    nt_OptionalU32_0._ipg_end += left;
    nt_OptionalU32_0._ipg_start += left;
    left = nt_OptionalU32_0._ipg_start;
    right = nt_OptionalU32_0._ipg_end;

    // { value = makeStringWithFormat(decodeAscii(Bytes@0.value), OptionalU32@0.value) }
    self.value = makeStringWithFormat(decodeAscii(nt_Bytes_0.value), nt_OptionalU32_0.value);

    // ExpectEmpty@0[OptionalU32@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_OptionalU32_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[OptionalU32@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TextFormat(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TextFormat",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_CrdtId_0;
    let nt_TaggedId_0;
    let nt_SubBlock_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // CrdtId@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "CrdtId@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CrdtId_0 = CrdtId(input, begin + left, begin + right);
    if (nt_CrdtId_0 === null) break _ipg_alt;
    if (nt_CrdtId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CrdtId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CrdtId_0._ipg_end);
    }
    nt_CrdtId_0._ipg_end += left;
    nt_CrdtId_0._ipg_start += left;
    left = nt_CrdtId_0._ipg_start;
    right = nt_CrdtId_0._ipg_end;

    // { charId = CrdtId@0.this }
    self.charId = (({_ipg_start,_ipg_end,...o}) => o)(nt_CrdtId_0);

    // TaggedId@0(1)[CrdtId@0.END, EOI]
    left = nt_CrdtId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[CrdtId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { timestamp = TaggedId@0.value }
    self.timestamp = nt_TaggedId_0.value;

    // SubBlock@0(2)[TaggedId@0.END, EOI]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "SubBlock@0(2)[TaggedId@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, 2);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // "\x11"[SubBlock@0.END, SubBlock@0.END + 1]
    left = nt_SubBlock_0._ipg_end;
    right = nt_SubBlock_0._ipg_end + 1;
    _ipg_failedTerm = { term: "\"\\x11\"[SubBlock@0.END, SubBlock@0.END + 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x11")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { formatCode = .[SubBlock@0.END + 1] }
    left = nt_SubBlock_0._ipg_end + 1;
    right = left + 1;
    _ipg_failedTerm = { term: "{ formatCode = .[SubBlock@0.END + 1] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.formatCode = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ExpectEmpty@0[SubBlock@0.END + 2, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end + 2;
    _ipg_failedTerm = { term: "ExpectEmpty@0[SubBlock@0.END + 2, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

    // { value = lwwU8(timestamp, formatCode < 0 || formatCode > 7 ? 1 : formatCode) }
    self.value = lwwU8(self.timestamp, self.formatCode < 0 || self.formatCode > 7 ? 1 : self.formatCode);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function UnknownBlock(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "UnknownBlock",
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

    // { data = *[0, EOI] }
    left = 0;
    right = EOI;
    _ipg_failedTerm = { term: "{ data = *[0, EOI] }", left, right };
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

function OptionalLWWBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalLWWBool",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWBool_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWBool@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LWWBool@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWBool_0 = LWWBool(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWBool_0 === null) break _ipg_alt;
    if (nt_LWWBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWBool_0._ipg_end);
    }
    nt_LWWBool_0._ipg_end += left;
    nt_LWWBool_0._ipg_start += left;
    left = nt_LWWBool_0._ipg_start;
    right = nt_LWWBool_0._ipg_end;

    // { value = LWWBool@0.value }
    self.value = nt_LWWBool_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LWWBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LWWBool",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_TaggedBool_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // TaggedBool@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedBool@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool_0 = TaggedBool(input, begin + left, begin + right, 2);
    if (nt_TaggedBool_0 === null) break _ipg_alt;
    if (nt_TaggedBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool_0._ipg_end);
    }
    nt_TaggedBool_0._ipg_end += left;
    nt_TaggedBool_0._ipg_start += left;
    left = nt_TaggedBool_0._ipg_start;
    right = nt_TaggedBool_0._ipg_end;

    // { value = lwwBool(TaggedId@0.value, TaggedBool@0.value) }
    self.value = lwwBool(nt_TaggedId_0.value, nt_TaggedBool_0.value);

    // ExpectEmpty@0[TaggedBool@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedBool_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedBool@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalLWWU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalLWWU8",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWU8_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWU8@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LWWU8@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWU8_0 = LWWU8(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWU8_0 === null) break _ipg_alt;
    if (nt_LWWU8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWU8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWU8_0._ipg_end);
    }
    nt_LWWU8_0._ipg_end += left;
    nt_LWWU8_0._ipg_start += left;
    left = nt_LWWU8_0._ipg_start;
    right = nt_LWWU8_0._ipg_end;

    // { value = LWWU8@0.value }
    self.value = nt_LWWU8_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LWWU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LWWU8",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_TaggedU8_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // TaggedU8@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedU8@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU8_0 = TaggedU8(input, begin + left, begin + right, 2);
    if (nt_TaggedU8_0 === null) break _ipg_alt;
    if (nt_TaggedU8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU8_0._ipg_end);
    }
    nt_TaggedU8_0._ipg_end += left;
    nt_TaggedU8_0._ipg_start += left;
    left = nt_TaggedU8_0._ipg_start;
    right = nt_TaggedU8_0._ipg_end;

    // { value = lwwU8(TaggedId@0.value, TaggedU8@0.value) }
    self.value = lwwU8(nt_TaggedId_0.value, nt_TaggedU8_0.value);

    // ExpectEmpty@0[TaggedU8@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedU8_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedU8@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalLWWFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalLWWFloat32",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWFloat32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWFloat32@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LWWFloat32@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWFloat32_0 = LWWFloat32(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWFloat32_0 === null) break _ipg_alt;
    if (nt_LWWFloat32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWFloat32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWFloat32_0._ipg_end);
    }
    nt_LWWFloat32_0._ipg_end += left;
    nt_LWWFloat32_0._ipg_start += left;
    left = nt_LWWFloat32_0._ipg_start;
    right = nt_LWWFloat32_0._ipg_end;

    // { value = LWWFloat32@0.value }
    self.value = nt_LWWFloat32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LWWFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LWWFloat32",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_TaggedFloat32_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // TaggedFloat32@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedFloat32@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedFloat32_0 = TaggedFloat32(input, begin + left, begin + right, 2);
    if (nt_TaggedFloat32_0 === null) break _ipg_alt;
    if (nt_TaggedFloat32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedFloat32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedFloat32_0._ipg_end);
    }
    nt_TaggedFloat32_0._ipg_end += left;
    nt_TaggedFloat32_0._ipg_start += left;
    left = nt_TaggedFloat32_0._ipg_start;
    right = nt_TaggedFloat32_0._ipg_end;

    // { value = lwwFloat32(TaggedId@0.value, TaggedFloat32@0.value) }
    self.value = lwwFloat32(nt_TaggedId_0.value, nt_TaggedFloat32_0.value);

    // ExpectEmpty@0[TaggedFloat32@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedFloat32_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedFloat32@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalLWWID(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalLWWID",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LWWID_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LWWID@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "LWWID@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LWWID_0 = LWWID(input, begin + left, begin + right, a_expectedIndex);
    if (nt_LWWID_0 === null) break _ipg_alt;
    if (nt_LWWID_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LWWID_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LWWID_0._ipg_end);
    }
    nt_LWWID_0._ipg_end += left;
    nt_LWWID_0._ipg_start += left;
    left = nt_LWWID_0._ipg_start;
    right = nt_LWWID_0._ipg_end;

    // { value = LWWID@0.value }
    self.value = nt_LWWID_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LWWID(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LWWID",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_TaggedId_1;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { timestamp = TaggedId@0.value }
    self.timestamp = nt_TaggedId_0.value;

    // TaggedId@1(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@1(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_1 = TaggedId(input, begin + left, begin + right, 2);
    if (nt_TaggedId_1 === null) break _ipg_alt;
    if (nt_TaggedId_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_1._ipg_end);
    }
    nt_TaggedId_1._ipg_end += left;
    nt_TaggedId_1._ipg_start += left;
    left = nt_TaggedId_1._ipg_start;
    right = nt_TaggedId_1._ipg_end;

    // { value = lwwCrdtId(timestamp, TaggedId@1.value) }
    self.value = lwwCrdtId(self.timestamp, nt_TaggedId_1.value);

    // ExpectEmpty@0[TaggedId@1.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_1._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[TaggedId@1.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function LWWString(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "LWWString",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_TaggedId_0;
    let nt_String_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "TaggedId@0(1)[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, 1);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // String@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_TaggedId_0._ipg_end;
    _ipg_failedTerm = { term: "String@0(2)[TaggedId@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = String(input, begin + left, begin + right, 2);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { value = lwwString(TaggedId@0.value, String@0.value) }
    self.value = lwwString(nt_TaggedId_0.value, nt_String_0.value);

    // ExpectEmpty@0[String@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_String_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[String@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function String(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "String",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_VarUInt_0;
    let nt_Bool_0;
    let nt_Bytes_0;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // Bool@0[VarUInt@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_VarUInt_0._ipg_end;
    _ipg_failedTerm = { term: "Bool@0[VarUInt@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool_0 = Bool(input, begin + left, begin + right);
    if (nt_Bool_0 === null) break _ipg_alt;
    if (nt_Bool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool_0._ipg_end);
    }
    nt_Bool_0._ipg_end += left;
    nt_Bool_0._ipg_start += left;
    left = nt_Bool_0._ipg_start;
    right = nt_Bool_0._ipg_end;

    // ?[ check(Bool@0.value, "String flag unset") ]
    _ipg_failedTerm = { term: "?[ check(Bool@0.value, \"String flag unset\") ]" };
    if (!check(nt_Bool_0.value, "String flag unset")) break _ipg_alt;

    // ?[ check(Bool@0.END + VarUInt@0.value <= SubBlock@0.END + SubBlock@0.length, "String: Overfull block") ]
    _ipg_failedTerm = { term: "?[ check(Bool@0.END + VarUInt@0.value <= SubBlock@0.END + SubBlock@0.length, \"String: Overfull block\") ]" };
    if (!check(nt_Bool_0._ipg_end + nt_VarUInt_0.value <= nt_SubBlock_0._ipg_end + nt_SubBlock_0.length, "String: Overfull block")) break _ipg_alt;

    // Bytes@0[Bool@0.END, Bool@0.END + VarUInt@0.value]
    left = nt_Bool_0._ipg_end;
    _ipg_failedTerm = { term: "Bytes@0[Bool@0.END, Bool@0.END + VarUInt@0.value]", left, right };
    right = nt_Bool_0._ipg_end + nt_VarUInt_0.value;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bytes_0 = Bytes(input, begin + left, begin + right);
    if (nt_Bytes_0 === null) break _ipg_alt;
    if (nt_Bytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bytes_0._ipg_end);
    }
    nt_Bytes_0._ipg_end += left;
    nt_Bytes_0._ipg_start += left;
    left = nt_Bytes_0._ipg_start;
    right = nt_Bytes_0._ipg_end;

    // { value = decodeAscii(Bytes@0.value) }
    self.value = decodeAscii(nt_Bytes_0.value);

    // ExpectEmpty@0[Bytes@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_Bytes_0._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[Bytes@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalU32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalU32",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedU32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedU32@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedU32@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedU32_0 = TaggedU32(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedU32_0 === null) break _ipg_alt;
    if (nt_TaggedU32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedU32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedU32_0._ipg_end);
    }
    nt_TaggedU32_0._ipg_end += left;
    nt_TaggedU32_0._ipg_start += left;
    left = nt_TaggedU32_0._ipg_start;
    right = nt_TaggedU32_0._ipg_end;

    // { value = TaggedU32@0.value }
    self.value = nt_TaggedU32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalIntPair(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalIntPair",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_IntPair_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // IntPair@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "IntPair@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_IntPair_0 = IntPair(input, begin + left, begin + right, a_expectedIndex);
    if (nt_IntPair_0 === null) break _ipg_alt;
    if (nt_IntPair_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_IntPair_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_IntPair_0._ipg_end);
    }
    nt_IntPair_0._ipg_end += left;
    nt_IntPair_0._ipg_start += left;
    left = nt_IntPair_0._ipg_start;
    right = nt_IntPair_0._ipg_end;

    // { value = IntPair@0.value }
    self.value = nt_IntPair_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function IntPair(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "IntPair",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SubBlock_0;
    let nt_U32_0;
    let nt_U32_1;
    let nt_ExpectEmpty_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SubBlock@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "SubBlock@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SubBlock_0 = SubBlock(input, begin + left, begin + right, a_expectedIndex);
    if (nt_SubBlock_0 === null) break _ipg_alt;
    if (nt_SubBlock_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SubBlock_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SubBlock_0._ipg_end);
    }
    nt_SubBlock_0._ipg_end += left;
    nt_SubBlock_0._ipg_start += left;
    left = nt_SubBlock_0._ipg_start;
    right = nt_SubBlock_0._ipg_end;

    // U32@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_SubBlock_0._ipg_end;
    _ipg_failedTerm = { term: "U32@0[SubBlock@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_0 = U32(input, begin + left, begin + right);
    if (nt_U32_0 === null) break _ipg_alt;
    if (nt_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_0._ipg_end);
    }
    nt_U32_0._ipg_end += left;
    nt_U32_0._ipg_start += left;
    left = nt_U32_0._ipg_start;
    right = nt_U32_0._ipg_end;

    // { fst = U32@0.value }
    self.fst = nt_U32_0.value;

    // U32@1[U32@0.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_U32_0._ipg_end;
    _ipg_failedTerm = { term: "U32@1[U32@0.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_1 = U32(input, begin + left, begin + right);
    if (nt_U32_1 === null) break _ipg_alt;
    if (nt_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_1._ipg_end);
    }
    nt_U32_1._ipg_end += left;
    nt_U32_1._ipg_start += left;
    left = nt_U32_1._ipg_start;
    right = nt_U32_1._ipg_end;

    // { value = makePair(fst, U32@1.value) }
    self.value = makePair(self.fst, nt_U32_1.value);

    // ExpectEmpty@0[U32@1.END, SubBlock@0.END + SubBlock@0.length]
    left = nt_U32_1._ipg_end;
    _ipg_failedTerm = { term: "ExpectEmpty@0[U32@1.END, SubBlock@0.END + SubBlock@0.length]", left, right };
    right = nt_SubBlock_0._ipg_end + nt_SubBlock_0.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_ExpectEmpty_0 = ExpectEmpty(input, begin + left, begin + right);
    if (nt_ExpectEmpty_0 === null) break _ipg_alt;
    if (nt_ExpectEmpty_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_ExpectEmpty_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_ExpectEmpty_0._ipg_end);
    }
    nt_ExpectEmpty_0._ipg_end += left;
    nt_ExpectEmpty_0._ipg_start += left;
    left = nt_ExpectEmpty_0._ipg_start;
    right = nt_ExpectEmpty_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function SubBlock(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "SubBlock",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Length4Tag_0;
    let nt_U32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Length4Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Length4Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Length4Tag_0 = Length4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Length4Tag_0 === null) break _ipg_alt;
    if (nt_Length4Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Length4Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Length4Tag_0._ipg_end);
    }
    nt_Length4Tag_0._ipg_end += left;
    nt_Length4Tag_0._ipg_start += left;
    left = nt_Length4Tag_0._ipg_start;
    right = nt_Length4Tag_0._ipg_end;

    // U32@0[Length4Tag@0.END, EOI]
    left = nt_Length4Tag_0._ipg_end;
    _ipg_failedTerm = { term: "U32@0[Length4Tag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_0 = U32(input, begin + left, begin + right);
    if (nt_U32_0 === null) break _ipg_alt;
    if (nt_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_0._ipg_end);
    }
    nt_U32_0._ipg_end += left;
    nt_U32_0._ipg_start += left;
    left = nt_U32_0._ipg_start;
    right = nt_U32_0._ipg_end;

    // { length = U32@0.value }
    self.length = nt_U32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function CrdtId(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "CrdtId",
    args: [],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { part1 = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ part1 = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.part1 = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // VarUInt@0[1, EOI]
    left = 1;
    _ipg_failedTerm = { term: "VarUInt@0[1, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // { part2 = VarUInt@0.value }
    self.part2 = nt_VarUInt_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalTaggedBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalTaggedBool",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedBool_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedBool@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedBool@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedBool_0 = TaggedBool(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedBool_0 === null) break _ipg_alt;
    if (nt_TaggedBool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedBool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedBool_0._ipg_end);
    }
    nt_TaggedBool_0._ipg_end += left;
    nt_TaggedBool_0._ipg_start += left;
    left = nt_TaggedBool_0._ipg_start;
    right = nt_TaggedBool_0._ipg_end;

    // { value = TaggedBool@0.value }
    self.value = nt_TaggedBool_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = !(!0) }
    self.value = !(!0);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TaggedBool(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedBool",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte1Tag_0;
    let nt_Bool_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte1Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Byte1Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte1Tag_0 = Byte1Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte1Tag_0 === null) break _ipg_alt;
    if (nt_Byte1Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte1Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte1Tag_0._ipg_end);
    }
    nt_Byte1Tag_0._ipg_end += left;
    nt_Byte1Tag_0._ipg_start += left;
    left = nt_Byte1Tag_0._ipg_start;
    right = nt_Byte1Tag_0._ipg_end;

    // Bool@0[Byte1Tag@0.END, EOI]
    left = nt_Byte1Tag_0._ipg_end;
    _ipg_failedTerm = { term: "Bool@0[Byte1Tag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Bool_0 = Bool(input, begin + left, begin + right);
    if (nt_Bool_0 === null) break _ipg_alt;
    if (nt_Bool_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Bool_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Bool_0._ipg_end);
    }
    nt_Bool_0._ipg_end += left;
    nt_Bool_0._ipg_start += left;
    left = nt_Bool_0._ipg_start;
    right = nt_Bool_0._ipg_end;

    // { value = Bool@0.value }
    self.value = nt_Bool_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TaggedU8(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedU8",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte1Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte1Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Byte1Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte1Tag_0 = Byte1Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte1Tag_0 === null) break _ipg_alt;
    if (nt_Byte1Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte1Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte1Tag_0._ipg_end);
    }
    nt_Byte1Tag_0._ipg_end += left;
    nt_Byte1Tag_0._ipg_start += left;
    left = nt_Byte1Tag_0._ipg_start;
    right = nt_Byte1Tag_0._ipg_end;

    // { value = .[Byte1Tag@0.END] }
    left = nt_Byte1Tag_0._ipg_end;
    right = left + 1;
    _ipg_failedTerm = { term: "{ value = .[Byte1Tag@0.END] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
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

function TaggedU32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedU32",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte4Tag_0;
    let nt_U32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte4Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Byte4Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte4Tag_0 = Byte4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte4Tag_0 === null) break _ipg_alt;
    if (nt_Byte4Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte4Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte4Tag_0._ipg_end);
    }
    nt_Byte4Tag_0._ipg_end += left;
    nt_Byte4Tag_0._ipg_start += left;
    left = nt_Byte4Tag_0._ipg_start;
    right = nt_Byte4Tag_0._ipg_end;

    // U32@0[Byte4Tag@0.END, EOI]
    left = nt_Byte4Tag_0._ipg_end;
    _ipg_failedTerm = { term: "U32@0[Byte4Tag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_0 = U32(input, begin + left, begin + right);
    if (nt_U32_0 === null) break _ipg_alt;
    if (nt_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_0._ipg_end);
    }
    nt_U32_0._ipg_end += left;
    nt_U32_0._ipg_start += left;
    left = nt_U32_0._ipg_start;
    right = nt_U32_0._ipg_end;

    // { value = U32@0.value }
    self.value = nt_U32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TaggedFloat32(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedFloat32",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte4Tag_0;
    let nt_Float32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte4Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Byte4Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte4Tag_0 = Byte4Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte4Tag_0 === null) break _ipg_alt;
    if (nt_Byte4Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte4Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte4Tag_0._ipg_end);
    }
    nt_Byte4Tag_0._ipg_end += left;
    nt_Byte4Tag_0._ipg_start += left;
    left = nt_Byte4Tag_0._ipg_start;
    right = nt_Byte4Tag_0._ipg_end;

    // Float32@0[Byte4Tag@0.END, EOI]
    left = nt_Byte4Tag_0._ipg_end;
    _ipg_failedTerm = { term: "Float32@0[Byte4Tag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float32_0 = Float32(input, begin + left, begin + right);
    if (nt_Float32_0 === null) break _ipg_alt;
    if (nt_Float32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float32_0._ipg_end);
    }
    nt_Float32_0._ipg_end += left;
    nt_Float32_0._ipg_start += left;
    left = nt_Float32_0._ipg_start;
    right = nt_Float32_0._ipg_end;

    // { value = Float32@0.value }
    self.value = nt_Float32_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TaggedFloat64(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedFloat64",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte8Tag_0;
    let nt_Float64_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte8Tag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Byte8Tag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte8Tag_0 = Byte8Tag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_Byte8Tag_0 === null) break _ipg_alt;
    if (nt_Byte8Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte8Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte8Tag_0._ipg_end);
    }
    nt_Byte8Tag_0._ipg_end += left;
    nt_Byte8Tag_0._ipg_start += left;
    left = nt_Byte8Tag_0._ipg_start;
    right = nt_Byte8Tag_0._ipg_end;

    // Float64@0[Byte8Tag@0.END, EOI]
    left = nt_Byte8Tag_0._ipg_end;
    _ipg_failedTerm = { term: "Float64@0[Byte8Tag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Float64_0 = Float64(input, begin + left, begin + right);
    if (nt_Float64_0 === null) break _ipg_alt;
    if (nt_Float64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Float64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Float64_0._ipg_end);
    }
    nt_Float64_0._ipg_end += left;
    nt_Float64_0._ipg_start += left;
    left = nt_Float64_0._ipg_start;
    right = nt_Float64_0._ipg_end;

    // { value = Float64@0.value }
    self.value = nt_Float64_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function OptionalTaggedId(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "OptionalTaggedId",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_TaggedId_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // TaggedId@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "TaggedId@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_TaggedId_0 = TaggedId(input, begin + left, begin + right, a_expectedIndex);
    if (nt_TaggedId_0 === null) break _ipg_alt;
    if (nt_TaggedId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_TaggedId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_TaggedId_0._ipg_end);
    }
    nt_TaggedId_0._ipg_end += left;
    nt_TaggedId_0._ipg_start += left;
    left = nt_TaggedId_0._ipg_start;
    right = nt_TaggedId_0._ipg_end;

    // { value = TaggedId@0.value }
    self.value = nt_TaggedId_0.value;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = getNull() }
    self.value = getNull();

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function TaggedId(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "TaggedId",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_IDTag_0;
    let nt_CrdtId_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // IDTag@0(expectedIndex)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "IDTag@0(expectedIndex)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_IDTag_0 = IDTag(input, begin + left, begin + right, a_expectedIndex);
    if (nt_IDTag_0 === null) break _ipg_alt;
    if (nt_IDTag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_IDTag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_IDTag_0._ipg_end);
    }
    nt_IDTag_0._ipg_end += left;
    nt_IDTag_0._ipg_start += left;
    left = nt_IDTag_0._ipg_start;
    right = nt_IDTag_0._ipg_end;

    // CrdtId@0[IDTag@0.END, EOI]
    left = nt_IDTag_0._ipg_end;
    _ipg_failedTerm = { term: "CrdtId@0[IDTag@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_CrdtId_0 = CrdtId(input, begin + left, begin + right);
    if (nt_CrdtId_0 === null) break _ipg_alt;
    if (nt_CrdtId_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_CrdtId_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_CrdtId_0._ipg_end);
    }
    nt_CrdtId_0._ipg_end += left;
    nt_CrdtId_0._ipg_start += left;
    left = nt_CrdtId_0._ipg_start;
    right = nt_CrdtId_0._ipg_end;

    // { value = makeCrdtId(CrdtId@0.part1, CrdtId@0.part2) }
    self.value = makeCrdtId(nt_CrdtId_0.part1, nt_CrdtId_0.part2);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function IDTag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "IDTag",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag@0(expectedIndex, 15)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Tag@0(expectedIndex, 15)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag_0 = Tag(input, begin + left, begin + right, a_expectedIndex, 15);
    if (nt_Tag_0 === null) break _ipg_alt;
    if (nt_Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag_0._ipg_end);
    }
    nt_Tag_0._ipg_end += left;
    nt_Tag_0._ipg_start += left;
    left = nt_Tag_0._ipg_start;
    right = nt_Tag_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Byte1Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Byte1Tag",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag@0(expectedIndex, 1)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Tag@0(expectedIndex, 1)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag_0 = Tag(input, begin + left, begin + right, a_expectedIndex, 1);
    if (nt_Tag_0 === null) break _ipg_alt;
    if (nt_Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag_0._ipg_end);
    }
    nt_Tag_0._ipg_end += left;
    nt_Tag_0._ipg_start += left;
    left = nt_Tag_0._ipg_start;
    right = nt_Tag_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Byte4Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Byte4Tag",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag@0(expectedIndex, 4)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Tag@0(expectedIndex, 4)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag_0 = Tag(input, begin + left, begin + right, a_expectedIndex, 4);
    if (nt_Tag_0 === null) break _ipg_alt;
    if (nt_Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag_0._ipg_end);
    }
    nt_Tag_0._ipg_end += left;
    nt_Tag_0._ipg_start += left;
    left = nt_Tag_0._ipg_start;
    right = nt_Tag_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Byte8Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Byte8Tag",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag@0(expectedIndex, 8)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Tag@0(expectedIndex, 8)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag_0 = Tag(input, begin + left, begin + right, a_expectedIndex, 8);
    if (nt_Tag_0 === null) break _ipg_alt;
    if (nt_Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag_0._ipg_end);
    }
    nt_Tag_0._ipg_end += left;
    nt_Tag_0._ipg_start += left;
    left = nt_Tag_0._ipg_start;
    right = nt_Tag_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Length4Tag(input, begin = 0, end = input.length, a_expectedIndex) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Length4Tag",
    args: [a_expectedIndex],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Tag_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Tag@0(expectedIndex, 12)[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "Tag@0(expectedIndex, 12)[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Tag_0 = Tag(input, begin + left, begin + right, a_expectedIndex, 12);
    if (nt_Tag_0 === null) break _ipg_alt;
    if (nt_Tag_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Tag_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Tag_0._ipg_end);
    }
    nt_Tag_0._ipg_end += left;
    nt_Tag_0._ipg_start += left;
    left = nt_Tag_0._ipg_start;
    right = nt_Tag_0._ipg_end;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Tag(input, begin = 0, end = input.length, a_expectedIndex, a_expectedTagType) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Tag",
    args: [a_expectedIndex, a_expectedTagType],
    begin,
    end,
    children: []
};
let _ipg_failedTerm = null;
_ipg_failTreeStack.push(_ipg_currentFailTree);
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VarUInt_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VarUInt@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "VarUInt@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // ?[ expectedIndex == VarUInt@0.value >> 4 ]
    _ipg_failedTerm = { term: "?[ expectedIndex == VarUInt@0.value >> 4 ]" };
    if (!(a_expectedIndex == nt_VarUInt_0.value >> 4)) break _ipg_alt;

    // ?[ expectedTagType == (VarUInt@0.value & 15) ]
    _ipg_failedTerm = { term: "?[ expectedTagType == (VarUInt@0.value & 15) ]" };
    if (!(a_expectedTagType == (nt_VarUInt_0.value & 15))) break _ipg_alt;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function ExpectEmpty(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "ExpectEmpty",
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

    // ?[ warnIf(EOI != 0, "Underfull block") ]
    _ipg_failedTerm = { term: "?[ warnIf(EOI != 0, \"Underfull block\") ]" };
    if (!warnIf(EOI != 0, "Underfull block")) break _ipg_alt;

    // { _ = *[0, EOI] }
    left = 0;
    right = EOI;
    _ipg_failedTerm = { term: "{ _ = *[0, EOI] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self._ = input.slice(begin + left, begin + right);
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

function Bytes(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Bytes",
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

    // { value = *[0, EOI] }
    left = 0;
    right = EOI;
    _ipg_failedTerm = { term: "{ value = *[0, EOI] }", left, right };
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

function Bool(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Bool",
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
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { value = !(!0) }
    self.value = !(!0);

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x01"[0, 1]
    left = 0;
    right = 1;
    _ipg_failedTerm = { term: "\"\\x01\"[0, 1]", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { value = !0 }
    self.value = !0;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function U8(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "U8",
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

    // { value = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ value = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
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

function U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "U32",
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

    // { bs = *[0, 4] }
    left = 0;
    right = 4;
    _ipg_failedTerm = { term: "{ bs = *[0, 4] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = bs[0] | bs[1] << 8 | bs[2] << 16 | bs[3] << 24 }
    self.value = self.bs[0] | self.bs[1] << 8 | self.bs[2] << 16 | self.bs[3] << 24;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Float32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Float32",
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

    // { bs = *[0, 4] }
    left = 0;
    right = 4;
    _ipg_failedTerm = { term: "{ bs = *[0, 4] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = toFloat32(bs) }
    self.value = toFloat32(self.bs);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function Float64(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "Float64",
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

    // { bs = *[0, 8] }
    left = 0;
    right = 8;
    _ipg_failedTerm = { term: "{ bs = *[0, 8] }", left, right };
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = toFloat64(bs) }
    self.value = toFloat64(self.bs);

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

function VarUInt(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  const _ipg_currentFailTree = {
    rule: "VarUInt",
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

    // { value = .[0] }
    left = 0;
    right = left + 1;
    _ipg_failedTerm = { term: "{ value = .[0] }", left, right };
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ value >> 7 == 0 ]
    _ipg_failedTerm = { term: "?[ value >> 7 == 0 ]" };
    if (!(self.value >> 7 == 0)) break _ipg_alt;

  _ipg_failTreeStack.pop();
    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U8_0;
    let nt_VarUInt_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U8@0[0, EOI]
    left = 0;
    _ipg_failedTerm = { term: "U8@0[0, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U8_0 = U8(input, begin + left, begin + right);
    if (nt_U8_0 === null) break _ipg_alt;
    if (nt_U8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8_0._ipg_end);
    }
    nt_U8_0._ipg_end += left;
    nt_U8_0._ipg_start += left;
    left = nt_U8_0._ipg_start;
    right = nt_U8_0._ipg_end;

    // VarUInt@0[U8@0.END, EOI]
    left = nt_U8_0._ipg_end;
    _ipg_failedTerm = { term: "VarUInt@0[U8@0.END, EOI]", left, right };
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VarUInt_0 = VarUInt(input, begin + left, begin + right);
    if (nt_VarUInt_0 === null) break _ipg_alt;
    if (nt_VarUInt_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VarUInt_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VarUInt_0._ipg_end);
    }
    nt_VarUInt_0._ipg_end += left;
    nt_VarUInt_0._ipg_start += left;
    left = nt_VarUInt_0._ipg_start;
    right = nt_VarUInt_0._ipg_end;

    // { value = U8@0.value & 127 | VarUInt@0.value << 7 }
    self.value = nt_U8_0.value & 127 | nt_VarUInt_0.value << 7;

  _ipg_failTreeStack.pop();
    return self;
  }

  _ipg_failTreeStack.pop();
_ipg_currentFailTree.failedTerm = _ipg_failedTerm;
_ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
  return null;
}

console.log(JSON.stringify(RM6(fs.readFileSync("./text_example.rm"))));
