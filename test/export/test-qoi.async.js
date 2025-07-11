const PImage = require("pureimage");
const fs = require("node:fs");

function render(width, height, colors, outFilePath) {
    const img = PImage.make(width, height);
    let i = 0;
    for (let y = 0; y < height; ++y) {
        for (let x = 0; x < width; ++x) {
            const color = colors[i++];
            img.setPixelRGBA_i(x, y, color.r, color.g, color.b, color.a);
        }
    }
    PImage.encodePNGToStream(img, fs.createWriteStream(outFilePath));
}

function initState() {
    const black = { r: 0, g: 0, b: 0, a: 255 };
    const transparent = { r: 0, g: 0, b: 0, a: 0 };
    const history = new Array(64);
    history.fill(transparent);
    return { prevColor: black, history };
}

function concat(runs) {
    return runs.flat();
}

function hash(color) {
    return (color.r * 3 + color.g * 5 + color.b * 7 + color.a * 11) % 64;
}

function rgbCase(state, r, g, b) {
    return rgbaCase(state, r, g, b, state.prevColor.a);
}

function rgbaCase(state, r, g, b, a) {
    const color = { r, g, b, a };
    state.prevColor = color;
    state.history[hash(color)] = color;
    return [color];
}

function indexCase(state, index) {
    const color = state.history[index];
    state.prevColor = color;
    return [color];
}

function diffCase(state, dr, dg, db) {
    return rgbCase(
        state,
        (state.prevColor.r + dr) & 0xFF,
        (state.prevColor.g + dg) & 0xFF,
        (state.prevColor.b + db) & 0xFF
    );
}

function lumaCase(state, diffGreen, drdg, dbdg) {
    return rgbCase(
        state,
        (state.prevColor.r + drdg + diffGreen) & 0xFF,
        (state.prevColor.g + diffGreen) & 0xFF,
        (state.prevColor.b + dbdg + diffGreen) & 0xFF
    );
}

function runCase(state, runLength) {
    const run = new Array(runLength);
    run.fill(state.prevColor);
    return run;
}
async function _ipg_startsWith(input, l, r, prefix) {
  if (r - l < prefix.length) return false;
  const s = await input.slice(l, l + prefix.length);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
async function QOI(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_QOIHeader_0;
    let nt_QOIChunks_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // QOIHeader@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_QOIHeader_0 = await QOIHeader(input, begin + left, begin + right);
    if (nt_QOIHeader_0 === null) break _ipg_alt;
    if (nt_QOIHeader_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIHeader_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIHeader_0._ipg_end);
    }
    nt_QOIHeader_0._ipg_end += left;
    nt_QOIHeader_0._ipg_start += left;
    left = nt_QOIHeader_0._ipg_start;
    right = nt_QOIHeader_0._ipg_end;

    // { width = QOIHeader@0.width }
    self.width = nt_QOIHeader_0.width;

    // { height = QOIHeader@0.height }
    self.height = nt_QOIHeader_0.height;

    // { channels = QOIHeader@0.channels }
    self.channels = nt_QOIHeader_0.channels;

    // { colorspace = QOIHeader@0.colorspace }
    self.colorspace = nt_QOIHeader_0.colorspace;

    // QOIChunks@0(initState())[QOIHeader@0.END, EOI]
    left = nt_QOIHeader_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_QOIChunks_0 = await QOIChunks(input, begin + left, begin + right, initState());
    if (nt_QOIChunks_0 === null) break _ipg_alt;
    if (nt_QOIChunks_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIChunks_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIChunks_0._ipg_end);
    }
    nt_QOIChunks_0._ipg_end += left;
    nt_QOIChunks_0._ipg_start += left;
    left = nt_QOIChunks_0._ipg_start;
    right = nt_QOIChunks_0._ipg_end;

    // { colors = concat(QOIChunks@0.values) }
    self.colors = concat(nt_QOIChunks_0.values);

    return self;
  }

  
  return null;
}

async function QOIHeader(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BE_U32_0;
    let nt_BE_U32_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "qoif"[0, 4]
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "qoif")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 4;
    self._ipg_end = Math.max(self._ipg_end, right);

    // BE_U32@0[4, EOI]
    left = 4;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U32_0 = await BE_U32(input, begin + left, begin + right);
    if (nt_BE_U32_0 === null) break _ipg_alt;
    if (nt_BE_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U32_0._ipg_end);
    }
    nt_BE_U32_0._ipg_end += left;
    nt_BE_U32_0._ipg_start += left;
    left = nt_BE_U32_0._ipg_start;
    right = nt_BE_U32_0._ipg_end;

    // { width = BE_U32@0.value }
    self.width = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, EOI]
    left = nt_BE_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U32_1 = await BE_U32(input, begin + left, begin + right);
    if (nt_BE_U32_1 === null) break _ipg_alt;
    if (nt_BE_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U32_1._ipg_end);
    }
    nt_BE_U32_1._ipg_end += left;
    nt_BE_U32_1._ipg_start += left;
    left = nt_BE_U32_1._ipg_start;
    right = nt_BE_U32_1._ipg_end;

    // { height = BE_U32@1.value }
    self.height = nt_BE_U32_1.value;

    // { channels = .[BE_U32@1.END] }
    left = nt_BE_U32_1._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.channels = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { colorspace = .[BE_U32@1.END + 1] }
    left = nt_BE_U32_1._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.colorspace = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function QOIChunks(input, begin = 0, end = input.length, a_state) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_QOIChunk_0;
    let nt_EndMarker_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat QOIChunk@0(state)[QOIChunk@0.END, EOI].chunk starting on [0, EOI] until EndMarker@0
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_EndMarker_0 = await EndMarker(input, begin + left, begin + right);
      if (nt_EndMarker_0 !== null) {
        if (nt_EndMarker_0._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_EndMarker_0._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_EndMarker_0._ipg_end);
        }
        nt_EndMarker_0._ipg_end += left;
        nt_EndMarker_0._ipg_start += left;
        right = nt_EndMarker_0._ipg_end;
        break;
      }
      nt_QOIChunk_0 = await QOIChunk(input, begin + left, begin + right, a_state);
      if (nt_QOIChunk_0 === null) break _ipg_alt;
      if (nt_QOIChunk_0._ipg_end === 0) throw 'repeat of non-consuming rule: QOIChunk';
      self._ipg_start = Math.min(self._ipg_start, left + nt_QOIChunk_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_QOIChunk_0._ipg_end);
      nt_QOIChunk_0._ipg_end += left;
      nt_QOIChunk_0._ipg_start += left;
      self.values.push(nt_QOIChunk_0.chunk);
      left = nt_QOIChunk_0._ipg_end;
      right = EOI;
    }

    return self;
  }

  
  return null;
}

async function QOIChunk(input, begin = 0, end = input.length, a_state) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Chunk_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { tagByte = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.tagByte = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // Chunk@0(state, tagByte)[1, EOI]
    left = 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Chunk_0 = await Chunk(input, begin + left, begin + right, a_state, self.tagByte);
    if (nt_Chunk_0 === null) break _ipg_alt;
    if (nt_Chunk_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Chunk_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Chunk_0._ipg_end);
    }
    nt_Chunk_0._ipg_end += left;
    nt_Chunk_0._ipg_start += left;
    left = nt_Chunk_0._ipg_start;
    right = nt_Chunk_0._ipg_end;

    // { chunk = Chunk@0.run }
    self.chunk = nt_Chunk_0.run;

    return self;
  }

  
  return null;
}

async function Chunk(input, begin = 0, end = input.length, a_state, a_tagByte) {
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

    // { run = rgbCase(state, r, g, b) }
    self.run = rgbCase(a_state, self.r, self.g, self.b);

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

    // { a = .[3] }
    left = 3;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.a = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { run = rgbaCase(state, r, g, b, a) }
    self.run = rgbaCase(a_state, self.r, self.g, self.b, self.a);

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

    // { run = indexCase(state, index) }
    self.run = indexCase(a_state, self.index);

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

    // { run = diffCase(state, dr, dg, db) }
    self.run = diffCase(a_state, self.dr, self.dg, self.db);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U8_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 2 ]
    if (!(a_tagByte >> 6 == 2)) break _ipg_alt;

    // { tag = "luma" }
    self.tag = "luma";

    // { diffGreen = (tagByte & 63) - 32 }
    self.diffGreen = (a_tagByte & 63) - 32;

    // U8@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U8_0 = await U8(input, begin + left, begin + right);
    if (nt_U8_0 === null) break _ipg_alt;
    if (nt_U8_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8_0._ipg_end);
    }
    nt_U8_0._ipg_end += left;
    nt_U8_0._ipg_start += left;
    left = nt_U8_0._ipg_start;
    right = nt_U8_0._ipg_end;

    // { drdg = (U8@0.value >> 4) - 8 }
    self.drdg = (nt_U8_0.value >> 4) - 8;

    // { dbdg = (U8@0.value & 15) - 8 }
    self.dbdg = (nt_U8_0.value & 15) - 8;

    // { run = lumaCase(state, diffGreen, drdg, dbdg) }
    self.run = lumaCase(a_state, self.diffGreen, self.drdg, self.dbdg);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ tagByte >> 6 == 3 ]
    if (!(a_tagByte >> 6 == 3)) break _ipg_alt;

    // { tag = "run" }
    self.tag = "run";

    // { runLength = (tagByte & 63) + 1 }
    self.runLength = (a_tagByte & 63) + 1;

    // { run = runCase(state, runLength) }
    self.run = runCase(a_state, self.runLength);

    return self;
  }

  
  return null;
}

async function EndMarker(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00\x00\x00\x00\x00\x00\x00\x01"[0, 8]
    left = 0;
    right = 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x00\x00\x00\x00\x00\x00\x00\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 8;
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function U8(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.value = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function BE_U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bs = *[0, 4] }
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bs = await input.slice(begin + left, begin + right);
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

const qoiData = QOI(fs.readFileSync("./qoi_test_images/dice.qoi"));
render(qoiData.width, qoiData.height, qoiData.colors, "./dice.png"); 
