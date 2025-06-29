const fs = require("node:fs");
function _ipg_startsWith(s, l, r, prefix) {
  if (r - l < prefix.length) return false;
  if (typeof s === 'string') return s.startsWith(prefix, l);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[l + i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
function QOI(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_QOIHeader;
    let nt_QOIChunks;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // QOIHeader[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_QOIHeader = QOIHeader(input, begin + left, begin + right);
    if (nt_QOIHeader === null) break _ipg_alt;
    if (nt_QOIHeader._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIHeader._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIHeader._ipg_end);
    }
    nt_QOIHeader._ipg_end += left;
    nt_QOIHeader._ipg_start += left;
    left = nt_QOIHeader._ipg_start;
    right = nt_QOIHeader._ipg_end;

    // { width = QOIHeader.width }
    self.width = nt_QOIHeader.width;

    // { height = QOIHeader.height }
    self.height = nt_QOIHeader.height;

    // { channels = QOIHeader.channels }
    self.channels = nt_QOIHeader.channels;

    // { colorspace = QOIHeader.colorspace }
    self.colorspace = nt_QOIHeader.colorspace;

    // QOIChunks[QOIHeader.END, EOI]
    left = nt_QOIHeader._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_QOIChunks = QOIChunks(input, begin + left, begin + right);
    if (nt_QOIChunks === null) break _ipg_alt;
    if (nt_QOIChunks._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIChunks._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIChunks._ipg_end);
    }
    nt_QOIChunks._ipg_end += left;
    nt_QOIChunks._ipg_start += left;
    left = nt_QOIChunks._ipg_start;
    right = nt_QOIChunks._ipg_end;

    // { chunks = QOIChunks.values }
    self.chunks = nt_QOIChunks.values;

    return self;
  }

  
  return null;
}

function QOIHeader(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BE_U32;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "qoif"[0, 4]
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "qoif")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 4;

    // BE_U32[4, EOI]
    left = 4;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U32 = BE_U32(input, begin + left, begin + right);
    if (nt_BE_U32 === null) break _ipg_alt;
    if (nt_BE_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U32._ipg_end);
    }
    nt_BE_U32._ipg_end += left;
    nt_BE_U32._ipg_start += left;
    left = nt_BE_U32._ipg_start;
    right = nt_BE_U32._ipg_end;

    // { width = BE_U32.value }
    self.width = nt_BE_U32.value;

    // BE_U32[BE_U32.END, EOI]
    left = nt_BE_U32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U32 = BE_U32(input, begin + left, begin + right);
    if (nt_BE_U32 === null) break _ipg_alt;
    if (nt_BE_U32._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U32._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U32._ipg_end);
    }
    nt_BE_U32._ipg_end += left;
    nt_BE_U32._ipg_start += left;
    left = nt_BE_U32._ipg_start;
    right = nt_BE_U32._ipg_end;

    // { height = BE_U32.value }
    self.height = nt_BE_U32.value;

    // { channels = .[BE_U32.END] }
    left = nt_BE_U32._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.channels = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { colorspace = .[BE_U32.END + 1] }
    left = nt_BE_U32._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.colorspace = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

function QOIChunks(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_QOIChunk;
    let nt_EndMarker;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat QOIChunk[QOIChunk.END, EOI].chunk starting on [0, EOI] until EndMarker
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_EndMarker = EndMarker(input, begin + left, begin + right);
      if (nt_EndMarker !== null) {
        if (nt_EndMarker._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_EndMarker._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_EndMarker._ipg_end);
        }
        nt_EndMarker._ipg_end += left;
        nt_EndMarker._ipg_start += left;
        right = nt_EndMarker._ipg_end;
        break;
      }
      nt_QOIChunk = QOIChunk(input, begin + left, begin + right);
      if (nt_QOIChunk === null) break _ipg_alt;
      if (nt_QOIChunk._ipg_end === 0) throw 'repeat of non-consuming rule: QOIChunk';
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIChunk._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIChunk._ipg_end);
      nt_QOIChunk._ipg_end += left;
      nt_QOIChunk._ipg_start += left;
      self.values.push(nt_QOIChunk.chunk);
      left = nt_QOIChunk._ipg_end;
      right = EOI;
    }

    return self;
  }

  
  return null;
}

function QOIChunk(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Chunk;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { tagByte = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.tagByte = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Chunk(tagByte)[1, EOI]
    left = 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Chunk = Chunk(input, begin + left, begin + right, self.tagByte);
    if (nt_Chunk === null) break _ipg_alt;
    if (nt_Chunk._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Chunk._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Chunk._ipg_end);
    }
    nt_Chunk._ipg_end += left;
    nt_Chunk._ipg_start += left;
    left = nt_Chunk._ipg_start;
    right = nt_Chunk._ipg_end;

    // { chunk = Chunk.this }
    self.chunk = (({_ipg_start,_ipg_end,...o}) => o)(nt_Chunk);

    return self;
  }

  
  return null;
}

function Chunk(input, begin = 0, end = input.length, a_tagByte) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte == 254 ]
    if (!(a_tagByte == 254)) break _ipg_alt;

    // { tag = "rgb" }
    self.tag = "rgb";

    // { r = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.r = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { g = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.g = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { b = .[2] }
    left = 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.b = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte == 255 ]
    if (!(a_tagByte == 255)) break _ipg_alt;

    // { tag = "rgba" }
    self.tag = "rgba";

    // { r = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.r = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { g = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.g = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { b = .[2] }
    left = 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.b = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { a = .[3] }
    left = 3;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.a = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 0 ]
    if (!(a_tagByte >> 6 == 0)) break _ipg_alt;

    // { tag = "index" }
    self.tag = "index";

    // { index = tagByte & 63 }
    self.index = a_tagByte & 63;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 1 ]
    if (!(a_tagByte >> 6 == 1)) break _ipg_alt;

    // { tag = "diff" }
    self.tag = "diff";

    // { dr = (tagByte >> 4 & 3) - 2 }
    self.dr = (a_tagByte >> 4 & 3) - 2;

    // { dg = (tagByte >> 2 & 3) - 2 }
    self.dg = (a_tagByte >> 2 & 3) - 2;

    // { db = (tagByte & 3) - 2 }
    self.db = (a_tagByte & 3) - 2;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U8;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 2 ]
    if (!(a_tagByte >> 6 == 2)) break _ipg_alt;

    // { tag = "luma" }
    self.tag = "luma";

    // { diffGreen = (tagByte & 63) - 32 }
    self.diffGreen = (a_tagByte & 63) - 32;

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

    // { drdg = (U8.value >> 4) - 8 }
    self.drdg = (nt_U8.value >> 4) - 8;

    // { dbdg = (U8.value & 15) - 8 }
    self.dbdg = (nt_U8.value & 15) - 8;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 3 ]
    if (!(a_tagByte >> 6 == 3)) break _ipg_alt;

    // { tag = "run" }
    self.tag = "run";

    // { run = (tagByte & 63) + 1 }
    self.run = (a_tagByte & 63) + 1;

    return self;
  }

  
  return null;
}

function EndMarker(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00\x00\x00\x00\x00\x00\x00\x01"[0, 8]
    left = 0;
    right = 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x00\x00\x00\x00\x00\x00\x00\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 8;

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

function BE_U32(input, begin = 0, end = input.length) {
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

    // { value = bs[3] | bs[2] << 8 | bs[1] << 16 | bs[0] << 24 }
    self.value = self.bs[3] | self.bs[2] << 8 | self.bs[1] << 16 | self.bs[0] << 24;

    return self;
  }

  
  return null;
}

console.log(JSON.stringify(QOI(fs.readFileSync("./qoi_test_images/dice.qoi"))));
