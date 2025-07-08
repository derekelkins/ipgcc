const BLOCK_SIZE = 4 * 1024;
const blockCache = {};

async function slice(uri, start, end) {
    if (start === end) return new Uint8Array();
    if (!(uri in blockCache)) {
        blockCache[uri] = {};
    }
    const cache = blockCache[uri];
    const block = Math.floor(start / BLOCK_SIZE);
    const numBlocks = Math.ceil((end - BLOCK_SIZE*block) / BLOCK_SIZE);
    const endBlock = block + numBlocks;
    const promises = [];
    for (let i = block; i < endBlock; ++i) {
        if (i in cache) continue;
        const j = i;
        promises.push(
            fetchRange(uri, BLOCK_SIZE * i, BLOCK_SIZE * (i + 1))
            .then((data) => { cache[j] = data; })
        );
    }
    const offset = start % BLOCK_SIZE;
    let targetOffset = BLOCK_SIZE - offset;
    const len = end - start;
    const tail = end % BLOCK_SIZE;
    const block1End = Math.min(BLOCK_SIZE, offset + len);
    const result = new Uint8Array(len);
    await Promise.all(promises);

    result.set(cache[block].slice(offset, block1End), 0);
    if (block + 1 === endBlock) return result;

    for (let i = block + 1; i < endBlock - 1; ++i) {
        result.set(cache[i], targetOffset);
        targetOffset += BLOCK_SIZE;
    }
    result.set(cache[block].slice(0, tail), targetOffset);
    return result;
}

export function clearCache(uri) {
    blockCache[uri] = {};
}

export async function fetchRange(uri, start, end) {
    const headers = new Headers({
        "Range": `bytes=${start}-${end-1}`
    });
    const request = new Request(uri, { headers });
    const result = await fetch(request);
    if (!result.ok) {
      console.log(result.status);
      return;
    }
    return result.bytes();
}

export async function contentLength(uri) {
    const response = await fetch(uri, { method: "HEAD" });
    const len = response.headers.get("Content-Length");
    return parseInt(len);
}

export class InputWrapper {
    constructor(uri) { this.uri = uri; }
    async at(l) { return (await this.slice(l, l+1))[0]; }
    slice(l, r) { return slice(this.uri, l, r); }
}

function decodeAscii(bs) {
    return new TextDecoder("ascii").decode(bs);
}

function decodeAscii2(bs) {
    return decodeAscii(new Uint8Array(bs));
}

function asHex(bs) {
    let result = "";
    for (let i = 0, length = bs.length; i < length; i++) {
      const hex = bs[i].toString(16);
      result += hex.length === 1 ? '0' + hex : hex;
    }
    return result;
}

function getPrimaryDescriptor(descriptors) {
    for (const descriptor of descriptors) {
        if (descriptor.descriptorType === "Primary Volume Descriptor") {
            return descriptor;
        }
    }
    return null;
}

function length(xs) { return xs.length; }

function get(o, f) { return o[f]; }

async function _ipg_startsWith(input, l, r, prefix) {
  if (r - l < prefix.length) return false;
  const s = await input.slice(l, l + prefix.length);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
const LOGICAL_SECTOR_SIZE = 2048;
export async function ISO9660(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VolumeDescriptors_0;
    let nt_DirectoriesRecursive_0;
    let nt_LPathTableRecords_0;
    let nt_DirectoryRecords_0;
    let seq_DirectoryRecords_0; let seq_DirectoryRecords_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VolumeDescriptors@0[16 * LOGICAL_SECTOR_SIZE, EOI]
    left = 16 * LOGICAL_SECTOR_SIZE;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VolumeDescriptors_0 = await VolumeDescriptors(input, begin + left, begin + right);
    if (nt_VolumeDescriptors_0 === null) break _ipg_alt;
    if (nt_VolumeDescriptors_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VolumeDescriptors_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VolumeDescriptors_0._ipg_end);
    }
    nt_VolumeDescriptors_0._ipg_end += left;
    nt_VolumeDescriptors_0._ipg_start += left;
    left = nt_VolumeDescriptors_0._ipg_start;
    right = nt_VolumeDescriptors_0._ipg_end;

    // { descriptors = VolumeDescriptors@0.values }
    self.descriptors = nt_VolumeDescriptors_0.values;

    // { primaryDescriptor = getPrimaryDescriptor(descriptors) }
    self.primaryDescriptor = getPrimaryDescriptor(self.descriptors);

    // { logicalBlockSize = get(primaryDescriptor, "logicalBlockSize") }
    self.logicalBlockSize = get(self.primaryDescriptor, "logicalBlockSize");

    // DirectoriesRecursive@0(logicalBlockSize, get(primaryDescriptor, "rootDirectoryRecord"))[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DirectoriesRecursive_0 = await DirectoriesRecursive(input, begin + left, begin + right, self.logicalBlockSize, get(self.primaryDescriptor, "rootDirectoryRecord"));
    if (nt_DirectoriesRecursive_0 === null) break _ipg_alt;
    if (nt_DirectoriesRecursive_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoriesRecursive_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoriesRecursive_0._ipg_end);
    }
    nt_DirectoriesRecursive_0._ipg_end += left;
    nt_DirectoriesRecursive_0._ipg_start += left;
    left = nt_DirectoriesRecursive_0._ipg_start;
    right = nt_DirectoriesRecursive_0._ipg_end;

    // { directoriesRecursive = DirectoriesRecursive@0.this }
    self.directoriesRecursive = (({_ipg_start,_ipg_end,...o}) => o)(nt_DirectoriesRecursive_0);

    // { pathTableLocation = get(primaryDescriptor, "locationOfTypeLPathTable") }
    self.pathTableLocation = get(self.primaryDescriptor, "locationOfTypeLPathTable");

    // { pathTableSize = get(primaryDescriptor, "pathTableSize") }
    self.pathTableSize = get(self.primaryDescriptor, "pathTableSize");

    // { pathTableOffset = logicalBlockSize * pathTableLocation }
    self.pathTableOffset = self.logicalBlockSize * self.pathTableLocation;

    // LPathTableRecords@0[pathTableOffset, pathTableOffset + pathTableSize]
    left = self.pathTableOffset;
    right = self.pathTableOffset + self.pathTableSize;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LPathTableRecords_0 = await LPathTableRecords(input, begin + left, begin + right);
    if (nt_LPathTableRecords_0 === null) break _ipg_alt;
    if (nt_LPathTableRecords_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LPathTableRecords_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LPathTableRecords_0._ipg_end);
    }
    nt_LPathTableRecords_0._ipg_end += left;
    nt_LPathTableRecords_0._ipg_start += left;
    left = nt_LPathTableRecords_0._ipg_start;
    right = nt_LPathTableRecords_0._ipg_end;

    // { paths = LPathTableRecords@0.values }
    self.paths = nt_LPathTableRecords_0.values;

    // for i = 0 to length(paths) do DirectoryRecords@0[logicalBlockSize * get(paths[i], "locationOfExtent"), logicalBlockSize * get(paths[i], "locationOfExtent") + LOGICAL_SECTOR_SIZE]
    nt_DirectoryRecords_0 = { _ipg_end: right, _ipg_start: left };
    seq_DirectoryRecords_0_start = 0;
    loopEnd = length(self.paths);
    seq_DirectoryRecords_0 = new Array(Math.max(0, loopEnd - seq_DirectoryRecords_0_start));
    for (let i_i = seq_DirectoryRecords_0_start; i_i < loopEnd; i_i++) {
      const left = self.logicalBlockSize * get(self.paths[i_i], "locationOfExtent");
      const right = self.logicalBlockSize * get(self.paths[i_i], "locationOfExtent") + LOGICAL_SECTOR_SIZE;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = await DirectoryRecords(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DirectoryRecords_0._ipg_end = tmp._ipg_end;
      nt_DirectoryRecords_0._ipg_start = tmp._ipg_start;
      seq_DirectoryRecords_0[i_i - seq_DirectoryRecords_0_start] = tmp;
    }
    left = nt_DirectoryRecords_0._ipg_start;
    right = nt_DirectoryRecords_0._ipg_end;

    // { directories = DirectoryRecords@0.these }
    self.directories = seq_DirectoryRecords_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

async function VolumeDescriptors(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VolumeDescriptor_0;
    let nt_VolumeDescriptorSetTerminator_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat VolumeDescriptor@0[VolumeDescriptor@0.START + LOGICAL_SECTOR_SIZE, VolumeDescriptor@0.START + 2 * LOGICAL_SECTOR_SIZE].descriptor starting on [0, LOGICAL_SECTOR_SIZE] until VolumeDescriptorSetTerminator@0
    left = 0;
    right = LOGICAL_SECTOR_SIZE;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_VolumeDescriptorSetTerminator_0 = await VolumeDescriptorSetTerminator(input, begin + left, begin + right);
      if (nt_VolumeDescriptorSetTerminator_0 !== null) {
        if (nt_VolumeDescriptorSetTerminator_0._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_VolumeDescriptorSetTerminator_0._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_VolumeDescriptorSetTerminator_0._ipg_end);
        }
        nt_VolumeDescriptorSetTerminator_0._ipg_end += left;
        nt_VolumeDescriptorSetTerminator_0._ipg_start += left;
        right = nt_VolumeDescriptorSetTerminator_0._ipg_end;
        break;
      }
      nt_VolumeDescriptor_0 = await VolumeDescriptor(input, begin + left, begin + right);
      if (nt_VolumeDescriptor_0 === null) break _ipg_alt;
      if (nt_VolumeDescriptor_0._ipg_end === 0) throw 'repeat of non-consuming rule: VolumeDescriptor';
      self._ipg_start = Math.min(self._ipg_start, left + nt_VolumeDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VolumeDescriptor_0._ipg_end);
      nt_VolumeDescriptor_0._ipg_end += left;
      nt_VolumeDescriptor_0._ipg_start += left;
      self.values.push(nt_VolumeDescriptor_0.descriptor);
      left = nt_VolumeDescriptor_0._ipg_start + LOGICAL_SECTOR_SIZE;
      right = nt_VolumeDescriptor_0._ipg_start + 2 * LOGICAL_SECTOR_SIZE;
    }

    return self;
  }

  
  return null;
}

async function VolumeDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_PrimaryVolumeDescriptor_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // PrimaryVolumeDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_PrimaryVolumeDescriptor_0 = await PrimaryVolumeDescriptor(input, begin + left, begin + right);
    if (nt_PrimaryVolumeDescriptor_0 === null) break _ipg_alt;
    if (nt_PrimaryVolumeDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_PrimaryVolumeDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_PrimaryVolumeDescriptor_0._ipg_end);
    }
    nt_PrimaryVolumeDescriptor_0._ipg_end += left;
    nt_PrimaryVolumeDescriptor_0._ipg_start += left;
    left = nt_PrimaryVolumeDescriptor_0._ipg_start;
    right = nt_PrimaryVolumeDescriptor_0._ipg_end;

    // { descriptor = PrimaryVolumeDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_PrimaryVolumeDescriptor_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_SupplementaryOrEnhancedVolumeDescriptor_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // SupplementaryOrEnhancedVolumeDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_SupplementaryOrEnhancedVolumeDescriptor_0 = await SupplementaryOrEnhancedVolumeDescriptor(input, begin + left, begin + right);
    if (nt_SupplementaryOrEnhancedVolumeDescriptor_0 === null) break _ipg_alt;
    if (nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_end);
    }
    nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_end += left;
    nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_start += left;
    left = nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_start;
    right = nt_SupplementaryOrEnhancedVolumeDescriptor_0._ipg_end;

    // { descriptor = SupplementaryOrEnhancedVolumeDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_SupplementaryOrEnhancedVolumeDescriptor_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VolumePartitionDescriptor_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VolumePartitionDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VolumePartitionDescriptor_0 = await VolumePartitionDescriptor(input, begin + left, begin + right);
    if (nt_VolumePartitionDescriptor_0 === null) break _ipg_alt;
    if (nt_VolumePartitionDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VolumePartitionDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VolumePartitionDescriptor_0._ipg_end);
    }
    nt_VolumePartitionDescriptor_0._ipg_end += left;
    nt_VolumePartitionDescriptor_0._ipg_start += left;
    left = nt_VolumePartitionDescriptor_0._ipg_start;
    right = nt_VolumePartitionDescriptor_0._ipg_end;

    // { descriptor = VolumePartitionDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_VolumePartitionDescriptor_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BootRecord_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // BootRecord@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BootRecord_0 = await BootRecord(input, begin + left, begin + right);
    if (nt_BootRecord_0 === null) break _ipg_alt;
    if (nt_BootRecord_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BootRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BootRecord_0._ipg_end);
    }
    nt_BootRecord_0._ipg_end += left;
    nt_BootRecord_0._ipg_start += left;
    left = nt_BootRecord_0._ipg_start;
    right = nt_BootRecord_0._ipg_end;

    // { descriptor = BootRecord@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_BootRecord_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_VolumeDescriptorSetTerminator_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // VolumeDescriptorSetTerminator@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_VolumeDescriptorSetTerminator_0 = await VolumeDescriptorSetTerminator(input, begin + left, begin + right);
    if (nt_VolumeDescriptorSetTerminator_0 === null) break _ipg_alt;
    if (nt_VolumeDescriptorSetTerminator_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_VolumeDescriptorSetTerminator_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_VolumeDescriptorSetTerminator_0._ipg_end);
    }
    nt_VolumeDescriptorSetTerminator_0._ipg_end += left;
    nt_VolumeDescriptorSetTerminator_0._ipg_start += left;
    left = nt_VolumeDescriptorSetTerminator_0._ipg_start;
    right = nt_VolumeDescriptorSetTerminator_0._ipg_end;

    // { descriptor = VolumeDescriptorSetTerminator@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_VolumeDescriptorSetTerminator_0);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_UnknownVolumeDescriptor_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // UnknownVolumeDescriptor@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_UnknownVolumeDescriptor_0 = await UnknownVolumeDescriptor(input, begin + left, begin + right);
    if (nt_UnknownVolumeDescriptor_0 === null) break _ipg_alt;
    if (nt_UnknownVolumeDescriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_UnknownVolumeDescriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_UnknownVolumeDescriptor_0._ipg_end);
    }
    nt_UnknownVolumeDescriptor_0._ipg_end += left;
    nt_UnknownVolumeDescriptor_0._ipg_start += left;
    left = nt_UnknownVolumeDescriptor_0._ipg_start;
    right = nt_UnknownVolumeDescriptor_0._ipg_end;

    // { descriptor = UnknownVolumeDescriptor@0.this }
    self.descriptor = (({_ipg_start,_ipg_end,...o}) => o)(nt_UnknownVolumeDescriptor_0);

    return self;
  }

  
  return null;
}

async function PrimaryVolumeDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_AChars_0;
    let nt_DChars_0;
    let nt_NULBytes_0;
    let nt_BB_U32_0;
    let nt_NULBytes_1;
    let nt_BB_U16_0;
    let nt_BB_U16_1;
    let nt_BB_U16_2;
    let nt_BB_U32_1;
    let nt_LE_U32_0;
    let nt_LE_U32_1;
    let nt_BE_U32_0;
    let nt_BE_U32_1;
    let nt_DirectoryRecord_0;
    let nt_DChars_1;
    let nt_AChars_1;
    let nt_AChars_2;
    let nt_AChars_3;
    let nt_DChars_2;
    let nt_DChars_3;
    let nt_DChars_4;
    let nt_DateAndTime_0;
    let nt_DateAndTime_1;
    let nt_DateAndTime_2;
    let nt_DateAndTime_3;
    let nt_HexBytes_0;
    let nt_NULBytes_2;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x01CD001\x01\x00"[0, 8]
    left = 0;
    right = 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x01CD001\x01\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 8;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { descriptorType = "Primary Volume Descriptor" }
    self.descriptorType = "Primary Volume Descriptor";

    // AChars@0[8, 40]
    left = 8;
    right = 40;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_0 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_0 === null) break _ipg_alt;
    if (nt_AChars_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_0._ipg_end);
    }
    nt_AChars_0._ipg_end += left;
    nt_AChars_0._ipg_start += left;
    left = nt_AChars_0._ipg_start;
    right = nt_AChars_0._ipg_end;

    // { systemIdentifier = AChars@0.value }
    self.systemIdentifier = nt_AChars_0.value;

    // DChars@0[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0._ipg_end;
    right = nt_AChars_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_0 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_0 === null) break _ipg_alt;
    if (nt_DChars_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_0._ipg_end);
    }
    nt_DChars_0._ipg_end += left;
    nt_DChars_0._ipg_start += left;
    left = nt_DChars_0._ipg_start;
    right = nt_DChars_0._ipg_end;

    // { volumeIdentifier = DChars@0.value }
    self.volumeIdentifier = nt_DChars_0.value;

    // NULBytes@0[DChars@0.END, DChars@0.END + 8]
    left = nt_DChars_0._ipg_end;
    right = nt_DChars_0._ipg_end + 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_0 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_0 === null) break _ipg_alt;
    if (nt_NULBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_0._ipg_end);
    }
    nt_NULBytes_0._ipg_end += left;
    nt_NULBytes_0._ipg_start += left;
    left = nt_NULBytes_0._ipg_start;
    right = nt_NULBytes_0._ipg_end;

    // BB_U32@0[NULBytes@0.END, EOI]
    left = nt_NULBytes_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_0 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_0 === null) break _ipg_alt;
    if (nt_BB_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_0._ipg_end);
    }
    nt_BB_U32_0._ipg_end += left;
    nt_BB_U32_0._ipg_start += left;
    left = nt_BB_U32_0._ipg_start;
    right = nt_BB_U32_0._ipg_end;

    // { volumeSpaceSize = BB_U32@0.value }
    self.volumeSpaceSize = nt_BB_U32_0.value;

    // NULBytes@1[BB_U32@0.END, BB_U32@0.END + 32]
    left = nt_BB_U32_0._ipg_end;
    right = nt_BB_U32_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_1 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_1 === null) break _ipg_alt;
    if (nt_NULBytes_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_1._ipg_end);
    }
    nt_NULBytes_1._ipg_end += left;
    nt_NULBytes_1._ipg_start += left;
    left = nt_NULBytes_1._ipg_start;
    right = nt_NULBytes_1._ipg_end;

    // BB_U16@0[NULBytes@1.END, NULBytes@1.END + 4]
    left = nt_NULBytes_1._ipg_end;
    right = nt_NULBytes_1._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_0 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_0 === null) break _ipg_alt;
    if (nt_BB_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_0._ipg_end);
    }
    nt_BB_U16_0._ipg_end += left;
    nt_BB_U16_0._ipg_start += left;
    left = nt_BB_U16_0._ipg_start;
    right = nt_BB_U16_0._ipg_end;

    // { volumeSetSize = BB_U16@0.value }
    self.volumeSetSize = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, BB_U16@0.END + 4]
    left = nt_BB_U16_0._ipg_end;
    right = nt_BB_U16_0._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_1 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_1 === null) break _ipg_alt;
    if (nt_BB_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_1._ipg_end);
    }
    nt_BB_U16_1._ipg_end += left;
    nt_BB_U16_1._ipg_start += left;
    left = nt_BB_U16_1._ipg_start;
    right = nt_BB_U16_1._ipg_end;

    // { volumeSequenceNumber = BB_U16@1.value }
    self.volumeSequenceNumber = nt_BB_U16_1.value;

    // BB_U16@2[BB_U16@1.END, BB_U16@1.END + 4]
    left = nt_BB_U16_1._ipg_end;
    right = nt_BB_U16_1._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_2 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_2 === null) break _ipg_alt;
    if (nt_BB_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_2._ipg_end);
    }
    nt_BB_U16_2._ipg_end += left;
    nt_BB_U16_2._ipg_start += left;
    left = nt_BB_U16_2._ipg_start;
    right = nt_BB_U16_2._ipg_end;

    // { logicalBlockSize = BB_U16@2.value }
    self.logicalBlockSize = nt_BB_U16_2.value;

    // BB_U32@1[BB_U16@2.END, BB_U16@2.END + 8]
    left = nt_BB_U16_2._ipg_end;
    right = nt_BB_U16_2._ipg_end + 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_1 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_1 === null) break _ipg_alt;
    if (nt_BB_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_1._ipg_end);
    }
    nt_BB_U32_1._ipg_end += left;
    nt_BB_U32_1._ipg_start += left;
    left = nt_BB_U32_1._ipg_start;
    right = nt_BB_U32_1._ipg_end;

    // { pathTableSize = BB_U32@1.value }
    self.pathTableSize = nt_BB_U32_1.value;

    // LE_U32@0[BB_U32@1.END, BB_U32@1.END + 4]
    left = nt_BB_U32_1._ipg_end;
    right = nt_BB_U32_1._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_0 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_0 === null) break _ipg_alt;
    if (nt_LE_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_0._ipg_end);
    }
    nt_LE_U32_0._ipg_end += left;
    nt_LE_U32_0._ipg_start += left;
    left = nt_LE_U32_0._ipg_start;
    right = nt_LE_U32_0._ipg_end;

    // { locationOfTypeLPathTable = LE_U32@0.value }
    self.locationOfTypeLPathTable = nt_LE_U32_0.value;

    // LE_U32@1[LE_U32@0.END, LE_U32@0.END + 4]
    left = nt_LE_U32_0._ipg_end;
    right = nt_LE_U32_0._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_1 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_1 === null) break _ipg_alt;
    if (nt_LE_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_1._ipg_end);
    }
    nt_LE_U32_1._ipg_end += left;
    nt_LE_U32_1._ipg_start += left;
    left = nt_LE_U32_1._ipg_start;
    right = nt_LE_U32_1._ipg_end;

    // { locationOfOptionalTypeLPathTable = LE_U32@1.value }
    self.locationOfOptionalTypeLPathTable = nt_LE_U32_1.value;

    // BE_U32@0[LE_U32@1.END, LE_U32@1.END + 4]
    left = nt_LE_U32_1._ipg_end;
    right = nt_LE_U32_1._ipg_end + 4;
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

    // { locationOfTypeMPathTable = BE_U32@0.value }
    self.locationOfTypeMPathTable = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, BE_U32@0.END + 4]
    left = nt_BE_U32_0._ipg_end;
    right = nt_BE_U32_0._ipg_end + 4;
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

    // { locationOfOptionalTypeMPathTable = BE_U32@1.value }
    self.locationOfOptionalTypeMPathTable = nt_BE_U32_1.value;

    // DirectoryRecord@0[BE_U32@1.END, BE_U32@1.END + 34]
    left = nt_BE_U32_1._ipg_end;
    right = nt_BE_U32_1._ipg_end + 34;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DirectoryRecord_0 = await DirectoryRecord(input, begin + left, begin + right);
    if (nt_DirectoryRecord_0 === null) break _ipg_alt;
    if (nt_DirectoryRecord_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoryRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoryRecord_0._ipg_end);
    }
    nt_DirectoryRecord_0._ipg_end += left;
    nt_DirectoryRecord_0._ipg_start += left;
    left = nt_DirectoryRecord_0._ipg_start;
    right = nt_DirectoryRecord_0._ipg_end;

    // { rootDirectoryRecord = DirectoryRecord@0.this }
    self.rootDirectoryRecord = (({_ipg_start,_ipg_end,...o}) => o)(nt_DirectoryRecord_0);

    // DChars@1[190, 318]
    left = 190;
    right = 318;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_1 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_1 === null) break _ipg_alt;
    if (nt_DChars_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_1._ipg_end);
    }
    nt_DChars_1._ipg_end += left;
    nt_DChars_1._ipg_start += left;
    left = nt_DChars_1._ipg_start;
    right = nt_DChars_1._ipg_end;

    // { volumeSetIdentifier = DChars@1.value }
    self.volumeSetIdentifier = nt_DChars_1.value;

    // AChars@1[DChars@1.END, DChars@1.END + 128]
    left = nt_DChars_1._ipg_end;
    right = nt_DChars_1._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_1 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_1 === null) break _ipg_alt;
    if (nt_AChars_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_1._ipg_end);
    }
    nt_AChars_1._ipg_end += left;
    nt_AChars_1._ipg_start += left;
    left = nt_AChars_1._ipg_start;
    right = nt_AChars_1._ipg_end;

    // { publisherIdentifier = AChars@1.value }
    self.publisherIdentifier = nt_AChars_1.value;

    // AChars@2[AChars@1.END, AChars@1.END + 128]
    left = nt_AChars_1._ipg_end;
    right = nt_AChars_1._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_2 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_2 === null) break _ipg_alt;
    if (nt_AChars_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_2._ipg_end);
    }
    nt_AChars_2._ipg_end += left;
    nt_AChars_2._ipg_start += left;
    left = nt_AChars_2._ipg_start;
    right = nt_AChars_2._ipg_end;

    // { dataPreparerIdentifier = AChars@2.value }
    self.dataPreparerIdentifier = nt_AChars_2.value;

    // AChars@3[AChars@2.END, AChars@2.END + 128]
    left = nt_AChars_2._ipg_end;
    right = nt_AChars_2._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_3 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_3 === null) break _ipg_alt;
    if (nt_AChars_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_3._ipg_end);
    }
    nt_AChars_3._ipg_end += left;
    nt_AChars_3._ipg_start += left;
    left = nt_AChars_3._ipg_start;
    right = nt_AChars_3._ipg_end;

    // { applicationIdentifier = AChars@3.value }
    self.applicationIdentifier = nt_AChars_3.value;

    // DChars@2[AChars@3.END, AChars@3.END + 37]
    left = nt_AChars_3._ipg_end;
    right = nt_AChars_3._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_2 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_2 === null) break _ipg_alt;
    if (nt_DChars_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_2._ipg_end);
    }
    nt_DChars_2._ipg_end += left;
    nt_DChars_2._ipg_start += left;
    left = nt_DChars_2._ipg_start;
    right = nt_DChars_2._ipg_end;

    // { copyrightFileIdentifier = DChars@2.value }
    self.copyrightFileIdentifier = nt_DChars_2.value;

    // DChars@3[DChars@2.END, DChars@2.END + 37]
    left = nt_DChars_2._ipg_end;
    right = nt_DChars_2._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_3 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_3 === null) break _ipg_alt;
    if (nt_DChars_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_3._ipg_end);
    }
    nt_DChars_3._ipg_end += left;
    nt_DChars_3._ipg_start += left;
    left = nt_DChars_3._ipg_start;
    right = nt_DChars_3._ipg_end;

    // { abstractFileIdentifier = DChars@3.value }
    self.abstractFileIdentifier = nt_DChars_3.value;

    // DChars@4[DChars@3.END, DChars@3.END + 37]
    left = nt_DChars_3._ipg_end;
    right = nt_DChars_3._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_4 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_4 === null) break _ipg_alt;
    if (nt_DChars_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_4._ipg_end);
    }
    nt_DChars_4._ipg_end += left;
    nt_DChars_4._ipg_start += left;
    left = nt_DChars_4._ipg_start;
    right = nt_DChars_4._ipg_end;

    // { bibliographicFileIdentifier = DChars@4.value }
    self.bibliographicFileIdentifier = nt_DChars_4.value;

    // DateAndTime@0[DChars@4.END, DChars@4.END + 17]
    left = nt_DChars_4._ipg_end;
    right = nt_DChars_4._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_0 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_0 === null) break _ipg_alt;
    if (nt_DateAndTime_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_0._ipg_end);
    }
    nt_DateAndTime_0._ipg_end += left;
    nt_DateAndTime_0._ipg_start += left;
    left = nt_DateAndTime_0._ipg_start;
    right = nt_DateAndTime_0._ipg_end;

    // { volumeCreationDateAndTime = DateAndTime@0.this }
    self.volumeCreationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_0);

    // DateAndTime@1[DateAndTime@0.END, DateAndTime@0.END + 17]
    left = nt_DateAndTime_0._ipg_end;
    right = nt_DateAndTime_0._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_1 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_1 === null) break _ipg_alt;
    if (nt_DateAndTime_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_1._ipg_end);
    }
    nt_DateAndTime_1._ipg_end += left;
    nt_DateAndTime_1._ipg_start += left;
    left = nt_DateAndTime_1._ipg_start;
    right = nt_DateAndTime_1._ipg_end;

    // { volumeModificationDateAndTime = DateAndTime@1.this }
    self.volumeModificationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_1);

    // DateAndTime@2[DateAndTime@1.END, DateAndTime@1.END + 17]
    left = nt_DateAndTime_1._ipg_end;
    right = nt_DateAndTime_1._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_2 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_2 === null) break _ipg_alt;
    if (nt_DateAndTime_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_2._ipg_end);
    }
    nt_DateAndTime_2._ipg_end += left;
    nt_DateAndTime_2._ipg_start += left;
    left = nt_DateAndTime_2._ipg_start;
    right = nt_DateAndTime_2._ipg_end;

    // { volumeExpirationDateAndTime = DateAndTime@2.this }
    self.volumeExpirationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_2);

    // DateAndTime@3[DateAndTime@2.END, DateAndTime@2.END + 17]
    left = nt_DateAndTime_2._ipg_end;
    right = nt_DateAndTime_2._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_3 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_3 === null) break _ipg_alt;
    if (nt_DateAndTime_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_3._ipg_end);
    }
    nt_DateAndTime_3._ipg_end += left;
    nt_DateAndTime_3._ipg_start += left;
    left = nt_DateAndTime_3._ipg_start;
    right = nt_DateAndTime_3._ipg_end;

    // { volumeEffectiveDateAndTime = DateAndTime@3.this }
    self.volumeEffectiveDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_3);

    // "\x01"[DateAndTime@3.END, DateAndTime@3.END + 1]
    left = nt_DateAndTime_3._ipg_end;
    right = nt_DateAndTime_3._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // "\x00"[DateAndTime@3.END + 1, DateAndTime@3.END + 2]
    left = nt_DateAndTime_3._ipg_end + 1;
    right = nt_DateAndTime_3._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // HexBytes@0[DateAndTime@3.END + 2, DateAndTime@3.END + 514]
    left = nt_DateAndTime_3._ipg_end + 2;
    right = nt_DateAndTime_3._ipg_end + 514;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { applicationUse = HexBytes@0.value }
    self.applicationUse = nt_HexBytes_0.value;

    // NULBytes@2[HexBytes@0.END, EOI]
    left = nt_HexBytes_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_2 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_2 === null) break _ipg_alt;
    if (nt_NULBytes_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_2._ipg_end);
    }
    nt_NULBytes_2._ipg_end += left;
    nt_NULBytes_2._ipg_start += left;
    left = nt_NULBytes_2._ipg_start;
    right = nt_NULBytes_2._ipg_end;

    return self;
  }

  
  return null;
}

async function SupplementaryOrEnhancedVolumeDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_String_0;
    let nt_String_1;
    let nt_NULBytes_0;
    let nt_BB_U32_0;
    let nt_HexBytes_0;
    let nt_BB_U16_0;
    let nt_BB_U16_1;
    let nt_BB_U16_2;
    let nt_BB_U32_1;
    let nt_LE_U32_0;
    let nt_LE_U32_1;
    let nt_BE_U32_0;
    let nt_BE_U32_1;
    let nt_DirectoryRecord_0;
    let nt_String_2;
    let nt_String_3;
    let nt_String_4;
    let nt_String_5;
    let nt_String_6;
    let nt_String_7;
    let nt_String_8;
    let nt_DateAndTime_0;
    let nt_DateAndTime_1;
    let nt_DateAndTime_2;
    let nt_DateAndTime_3;
    let nt_HexBytes_1;
    let nt_NULBytes_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x02CD001"[0, 6]
    left = 0;
    right = 6;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x02CD001")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 6;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { volumeDescriptorVersion = .[6] }
    left = 6;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.volumeDescriptorVersion = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ volumeDescriptorVersion == 1 || volumeDescriptorVersion == 2 ]
    if (!(self.volumeDescriptorVersion == 1 || self.volumeDescriptorVersion == 2)) break _ipg_alt;

    // { descriptorType = volumeDescriptorVersion == 1 ? "Supplementary Volume Descriptor" : "Enhanced Volume Descriptor" }
    self.descriptorType = self.volumeDescriptorVersion == 1 ? "Supplementary Volume Descriptor" : "Enhanced Volume Descriptor";

    // { volumeFlags = .[7] }
    left = 7;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.volumeFlags = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // String@0[8, 40]
    left = 8;
    right = 40;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = await String(input, begin + left, begin + right);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { systemIdentifier = String@0.value }
    self.systemIdentifier = nt_String_0.value;

    // String@1[String@0.END, String@0.END + 32]
    left = nt_String_0._ipg_end;
    right = nt_String_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_1 = await String(input, begin + left, begin + right);
    if (nt_String_1 === null) break _ipg_alt;
    if (nt_String_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_1._ipg_end);
    }
    nt_String_1._ipg_end += left;
    nt_String_1._ipg_start += left;
    left = nt_String_1._ipg_start;
    right = nt_String_1._ipg_end;

    // { volumeIdentifier = String@1.value }
    self.volumeIdentifier = nt_String_1.value;

    // NULBytes@0[String@1.END, String@1.END + 8]
    left = nt_String_1._ipg_end;
    right = nt_String_1._ipg_end + 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_0 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_0 === null) break _ipg_alt;
    if (nt_NULBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_0._ipg_end);
    }
    nt_NULBytes_0._ipg_end += left;
    nt_NULBytes_0._ipg_start += left;
    left = nt_NULBytes_0._ipg_start;
    right = nt_NULBytes_0._ipg_end;

    // BB_U32@0[NULBytes@0.END, EOI]
    left = nt_NULBytes_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_0 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_0 === null) break _ipg_alt;
    if (nt_BB_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_0._ipg_end);
    }
    nt_BB_U32_0._ipg_end += left;
    nt_BB_U32_0._ipg_start += left;
    left = nt_BB_U32_0._ipg_start;
    right = nt_BB_U32_0._ipg_end;

    // { volumeSpaceSize = BB_U32@0.value }
    self.volumeSpaceSize = nt_BB_U32_0.value;

    // HexBytes@0[BB_U32@0.END, BB_U32@0.END + 32]
    left = nt_BB_U32_0._ipg_end;
    right = nt_BB_U32_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { escapeSequences = HexBytes@0.value }
    self.escapeSequences = nt_HexBytes_0.value;

    // BB_U16@0[HexBytes@0.END, HexBytes@0.END + 4]
    left = nt_HexBytes_0._ipg_end;
    right = nt_HexBytes_0._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_0 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_0 === null) break _ipg_alt;
    if (nt_BB_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_0._ipg_end);
    }
    nt_BB_U16_0._ipg_end += left;
    nt_BB_U16_0._ipg_start += left;
    left = nt_BB_U16_0._ipg_start;
    right = nt_BB_U16_0._ipg_end;

    // { volumeSetSize = BB_U16@0.value }
    self.volumeSetSize = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, BB_U16@0.END + 4]
    left = nt_BB_U16_0._ipg_end;
    right = nt_BB_U16_0._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_1 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_1 === null) break _ipg_alt;
    if (nt_BB_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_1._ipg_end);
    }
    nt_BB_U16_1._ipg_end += left;
    nt_BB_U16_1._ipg_start += left;
    left = nt_BB_U16_1._ipg_start;
    right = nt_BB_U16_1._ipg_end;

    // { volumeSequenceNumber = BB_U16@1.value }
    self.volumeSequenceNumber = nt_BB_U16_1.value;

    // BB_U16@2[BB_U16@1.END, BB_U16@1.END + 4]
    left = nt_BB_U16_1._ipg_end;
    right = nt_BB_U16_1._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_2 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_2 === null) break _ipg_alt;
    if (nt_BB_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_2._ipg_end);
    }
    nt_BB_U16_2._ipg_end += left;
    nt_BB_U16_2._ipg_start += left;
    left = nt_BB_U16_2._ipg_start;
    right = nt_BB_U16_2._ipg_end;

    // { logicalBlockSize = BB_U16@2.value }
    self.logicalBlockSize = nt_BB_U16_2.value;

    // BB_U32@1[BB_U16@2.END, BB_U16@2.END + 8]
    left = nt_BB_U16_2._ipg_end;
    right = nt_BB_U16_2._ipg_end + 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_1 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_1 === null) break _ipg_alt;
    if (nt_BB_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_1._ipg_end);
    }
    nt_BB_U32_1._ipg_end += left;
    nt_BB_U32_1._ipg_start += left;
    left = nt_BB_U32_1._ipg_start;
    right = nt_BB_U32_1._ipg_end;

    // { pathTableSize = BB_U32@1.value }
    self.pathTableSize = nt_BB_U32_1.value;

    // LE_U32@0[BB_U32@1.END, BB_U32@1.END + 4]
    left = nt_BB_U32_1._ipg_end;
    right = nt_BB_U32_1._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_0 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_0 === null) break _ipg_alt;
    if (nt_LE_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_0._ipg_end);
    }
    nt_LE_U32_0._ipg_end += left;
    nt_LE_U32_0._ipg_start += left;
    left = nt_LE_U32_0._ipg_start;
    right = nt_LE_U32_0._ipg_end;

    // { locationOfTypeLPathTable = LE_U32@0.value }
    self.locationOfTypeLPathTable = nt_LE_U32_0.value;

    // LE_U32@1[LE_U32@0.END, LE_U32@0.END + 4]
    left = nt_LE_U32_0._ipg_end;
    right = nt_LE_U32_0._ipg_end + 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_1 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_1 === null) break _ipg_alt;
    if (nt_LE_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_1._ipg_end);
    }
    nt_LE_U32_1._ipg_end += left;
    nt_LE_U32_1._ipg_start += left;
    left = nt_LE_U32_1._ipg_start;
    right = nt_LE_U32_1._ipg_end;

    // { locationOfOptionalTypeLPathTable = LE_U32@1.value }
    self.locationOfOptionalTypeLPathTable = nt_LE_U32_1.value;

    // BE_U32@0[LE_U32@1.END, LE_U32@1.END + 4]
    left = nt_LE_U32_1._ipg_end;
    right = nt_LE_U32_1._ipg_end + 4;
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

    // { locationOfTypeMPathTable = BE_U32@0.value }
    self.locationOfTypeMPathTable = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, BE_U32@0.END + 4]
    left = nt_BE_U32_0._ipg_end;
    right = nt_BE_U32_0._ipg_end + 4;
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

    // { locationOfOptionalTypeMPathTable = BE_U32@1.value }
    self.locationOfOptionalTypeMPathTable = nt_BE_U32_1.value;

    // DirectoryRecord@0[BE_U32@1.END, BE_U32@1.END + 34]
    left = nt_BE_U32_1._ipg_end;
    right = nt_BE_U32_1._ipg_end + 34;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DirectoryRecord_0 = await DirectoryRecord(input, begin + left, begin + right);
    if (nt_DirectoryRecord_0 === null) break _ipg_alt;
    if (nt_DirectoryRecord_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoryRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoryRecord_0._ipg_end);
    }
    nt_DirectoryRecord_0._ipg_end += left;
    nt_DirectoryRecord_0._ipg_start += left;
    left = nt_DirectoryRecord_0._ipg_start;
    right = nt_DirectoryRecord_0._ipg_end;

    // { rootDirectoryRecord = DirectoryRecord@0.this }
    self.rootDirectoryRecord = (({_ipg_start,_ipg_end,...o}) => o)(nt_DirectoryRecord_0);

    // String@2[190, 318]
    left = 190;
    right = 318;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_2 = await String(input, begin + left, begin + right);
    if (nt_String_2 === null) break _ipg_alt;
    if (nt_String_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_2._ipg_end);
    }
    nt_String_2._ipg_end += left;
    nt_String_2._ipg_start += left;
    left = nt_String_2._ipg_start;
    right = nt_String_2._ipg_end;

    // { volumeSetIdentifier = String@2.value }
    self.volumeSetIdentifier = nt_String_2.value;

    // String@3[String@2.END, String@2.END + 128]
    left = nt_String_2._ipg_end;
    right = nt_String_2._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_3 = await String(input, begin + left, begin + right);
    if (nt_String_3 === null) break _ipg_alt;
    if (nt_String_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_3._ipg_end);
    }
    nt_String_3._ipg_end += left;
    nt_String_3._ipg_start += left;
    left = nt_String_3._ipg_start;
    right = nt_String_3._ipg_end;

    // { publisherIdentifier = String@3.value }
    self.publisherIdentifier = nt_String_3.value;

    // String@4[String@3.END, String@3.END + 128]
    left = nt_String_3._ipg_end;
    right = nt_String_3._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_4 = await String(input, begin + left, begin + right);
    if (nt_String_4 === null) break _ipg_alt;
    if (nt_String_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_4._ipg_end);
    }
    nt_String_4._ipg_end += left;
    nt_String_4._ipg_start += left;
    left = nt_String_4._ipg_start;
    right = nt_String_4._ipg_end;

    // { dataPreparerIdentifier = String@4.value }
    self.dataPreparerIdentifier = nt_String_4.value;

    // String@5[String@4.END, String@4.END + 128]
    left = nt_String_4._ipg_end;
    right = nt_String_4._ipg_end + 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_5 = await String(input, begin + left, begin + right);
    if (nt_String_5 === null) break _ipg_alt;
    if (nt_String_5._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_5._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_5._ipg_end);
    }
    nt_String_5._ipg_end += left;
    nt_String_5._ipg_start += left;
    left = nt_String_5._ipg_start;
    right = nt_String_5._ipg_end;

    // { applicationIdentifier = String@5.value }
    self.applicationIdentifier = nt_String_5.value;

    // String@6[String@5.END, String@5.END + 37]
    left = nt_String_5._ipg_end;
    right = nt_String_5._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_6 = await String(input, begin + left, begin + right);
    if (nt_String_6 === null) break _ipg_alt;
    if (nt_String_6._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_6._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_6._ipg_end);
    }
    nt_String_6._ipg_end += left;
    nt_String_6._ipg_start += left;
    left = nt_String_6._ipg_start;
    right = nt_String_6._ipg_end;

    // { copyrightFileIdentifier = String@6.value }
    self.copyrightFileIdentifier = nt_String_6.value;

    // String@7[String@6.END, String@6.END + 37]
    left = nt_String_6._ipg_end;
    right = nt_String_6._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_7 = await String(input, begin + left, begin + right);
    if (nt_String_7 === null) break _ipg_alt;
    if (nt_String_7._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_7._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_7._ipg_end);
    }
    nt_String_7._ipg_end += left;
    nt_String_7._ipg_start += left;
    left = nt_String_7._ipg_start;
    right = nt_String_7._ipg_end;

    // { abstractFileIdentifier = String@7.value }
    self.abstractFileIdentifier = nt_String_7.value;

    // String@8[String@7.END, String@7.END + 37]
    left = nt_String_7._ipg_end;
    right = nt_String_7._ipg_end + 37;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_8 = await String(input, begin + left, begin + right);
    if (nt_String_8 === null) break _ipg_alt;
    if (nt_String_8._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_8._ipg_end);
    }
    nt_String_8._ipg_end += left;
    nt_String_8._ipg_start += left;
    left = nt_String_8._ipg_start;
    right = nt_String_8._ipg_end;

    // { bibliographicFileIdentifier = String@8.value }
    self.bibliographicFileIdentifier = nt_String_8.value;

    // DateAndTime@0[String@8.END, String@8.END + 17]
    left = nt_String_8._ipg_end;
    right = nt_String_8._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_0 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_0 === null) break _ipg_alt;
    if (nt_DateAndTime_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_0._ipg_end);
    }
    nt_DateAndTime_0._ipg_end += left;
    nt_DateAndTime_0._ipg_start += left;
    left = nt_DateAndTime_0._ipg_start;
    right = nt_DateAndTime_0._ipg_end;

    // { volumeCreationDateAndTime = DateAndTime@0.this }
    self.volumeCreationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_0);

    // DateAndTime@1[DateAndTime@0.END, DateAndTime@0.END + 17]
    left = nt_DateAndTime_0._ipg_end;
    right = nt_DateAndTime_0._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_1 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_1 === null) break _ipg_alt;
    if (nt_DateAndTime_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_1._ipg_end);
    }
    nt_DateAndTime_1._ipg_end += left;
    nt_DateAndTime_1._ipg_start += left;
    left = nt_DateAndTime_1._ipg_start;
    right = nt_DateAndTime_1._ipg_end;

    // { volumeModificationDateAndTime = DateAndTime@1.this }
    self.volumeModificationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_1);

    // DateAndTime@2[DateAndTime@1.END, DateAndTime@1.END + 17]
    left = nt_DateAndTime_1._ipg_end;
    right = nt_DateAndTime_1._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_2 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_2 === null) break _ipg_alt;
    if (nt_DateAndTime_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_2._ipg_end);
    }
    nt_DateAndTime_2._ipg_end += left;
    nt_DateAndTime_2._ipg_start += left;
    left = nt_DateAndTime_2._ipg_start;
    right = nt_DateAndTime_2._ipg_end;

    // { volumeExpirationDateAndTime = DateAndTime@2.this }
    self.volumeExpirationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_2);

    // DateAndTime@3[DateAndTime@2.END, DateAndTime@2.END + 17]
    left = nt_DateAndTime_2._ipg_end;
    right = nt_DateAndTime_2._ipg_end + 17;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_3 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_3 === null) break _ipg_alt;
    if (nt_DateAndTime_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_3._ipg_end);
    }
    nt_DateAndTime_3._ipg_end += left;
    nt_DateAndTime_3._ipg_start += left;
    left = nt_DateAndTime_3._ipg_start;
    right = nt_DateAndTime_3._ipg_end;

    // { volumeEffectiveDateAndTime = DateAndTime@3.this }
    self.volumeEffectiveDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_3);

    // { fileStructureVersion = .[DateAndTime@3.END] }
    left = nt_DateAndTime_3._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.fileStructureVersion = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // "\x00"[DateAndTime@3.END + 1, DateAndTime@3.END + 2]
    left = nt_DateAndTime_3._ipg_end + 1;
    right = nt_DateAndTime_3._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // HexBytes@1[DateAndTime@3.END + 2, DateAndTime@3.END + 514]
    left = nt_DateAndTime_3._ipg_end + 2;
    right = nt_DateAndTime_3._ipg_end + 514;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_1 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_1 === null) break _ipg_alt;
    if (nt_HexBytes_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_1._ipg_end);
    }
    nt_HexBytes_1._ipg_end += left;
    nt_HexBytes_1._ipg_start += left;
    left = nt_HexBytes_1._ipg_start;
    right = nt_HexBytes_1._ipg_end;

    // { applicationUse = HexBytes@1.value }
    self.applicationUse = nt_HexBytes_1.value;

    // NULBytes@1[HexBytes@1.END, EOI]
    left = nt_HexBytes_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_1 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_1 === null) break _ipg_alt;
    if (nt_NULBytes_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_1._ipg_end);
    }
    nt_NULBytes_1._ipg_end += left;
    nt_NULBytes_1._ipg_start += left;
    left = nt_NULBytes_1._ipg_start;
    right = nt_NULBytes_1._ipg_end;

    return self;
  }

  
  return null;
}

async function VolumePartitionDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_AChars_0;
    let nt_DChars_0;
    let nt_BB_U32_0;
    let nt_BB_U32_1;
    let nt_HexBytes_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x03CD001\x01\x00"[0, 8]
    left = 0;
    right = 8;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x03CD001\x01\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 8;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { descriptorType = "Volume Partition Descriptor" }
    self.descriptorType = "Volume Partition Descriptor";

    // AChars@0[8, 40]
    left = 8;
    right = 40;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_0 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_0 === null) break _ipg_alt;
    if (nt_AChars_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_0._ipg_end);
    }
    nt_AChars_0._ipg_end += left;
    nt_AChars_0._ipg_start += left;
    left = nt_AChars_0._ipg_start;
    right = nt_AChars_0._ipg_end;

    // { systemIdentifier = AChars@0.value }
    self.systemIdentifier = nt_AChars_0.value;

    // DChars@0[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0._ipg_end;
    right = nt_AChars_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DChars_0 = await DChars(input, begin + left, begin + right);
    if (nt_DChars_0 === null) break _ipg_alt;
    if (nt_DChars_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DChars_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DChars_0._ipg_end);
    }
    nt_DChars_0._ipg_end += left;
    nt_DChars_0._ipg_start += left;
    left = nt_DChars_0._ipg_start;
    right = nt_DChars_0._ipg_end;

    // { volumePartitionIdentifier = DChars@0.value }
    self.volumePartitionIdentifier = nt_DChars_0.value;

    // BB_U32@0[DChars@0.END, EOI]
    left = nt_DChars_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_0 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_0 === null) break _ipg_alt;
    if (nt_BB_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_0._ipg_end);
    }
    nt_BB_U32_0._ipg_end += left;
    nt_BB_U32_0._ipg_start += left;
    left = nt_BB_U32_0._ipg_start;
    right = nt_BB_U32_0._ipg_end;

    // { volumePartitionLocation = BB_U32@0.value }
    self.volumePartitionLocation = nt_BB_U32_0.value;

    // BB_U32@1[BB_U32@0.END, EOI]
    left = nt_BB_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_1 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_1 === null) break _ipg_alt;
    if (nt_BB_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_1._ipg_end);
    }
    nt_BB_U32_1._ipg_end += left;
    nt_BB_U32_1._ipg_start += left;
    left = nt_BB_U32_1._ipg_start;
    right = nt_BB_U32_1._ipg_end;

    // { volumePartitionSize = BB_U32@1.value }
    self.volumePartitionSize = nt_BB_U32_1.value;

    // HexBytes@0[BB_U32@1.END, EOI]
    left = nt_BB_U32_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { systemUse = HexBytes@0.value }
    self.systemUse = nt_HexBytes_0.value;

    return self;
  }

  
  return null;
}

async function BootRecord(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_AChars_0;
    let nt_AChars_1;
    let nt_HexBytes_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x00CD001\x01"[0, 7]
    left = 0;
    right = 7;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x00CD001\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 7;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { descriptorType = "Boot Record" }
    self.descriptorType = "Boot Record";

    // AChars@0[7, 39]
    left = 7;
    right = 39;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_0 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_0 === null) break _ipg_alt;
    if (nt_AChars_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_0._ipg_end);
    }
    nt_AChars_0._ipg_end += left;
    nt_AChars_0._ipg_start += left;
    left = nt_AChars_0._ipg_start;
    right = nt_AChars_0._ipg_end;

    // { bootSystemIdentifier = AChars@0.value }
    self.bootSystemIdentifier = nt_AChars_0.value;

    // AChars@1[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0._ipg_end;
    right = nt_AChars_0._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_AChars_1 = await AChars(input, begin + left, begin + right);
    if (nt_AChars_1 === null) break _ipg_alt;
    if (nt_AChars_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_AChars_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_AChars_1._ipg_end);
    }
    nt_AChars_1._ipg_end += left;
    nt_AChars_1._ipg_start += left;
    left = nt_AChars_1._ipg_start;
    right = nt_AChars_1._ipg_end;

    // { bootIdentifier = AChars@1.value }
    self.bootIdentifier = nt_AChars_1.value;

    // HexBytes@0[AChars@1.END, EOI]
    left = nt_AChars_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { systemUse = HexBytes@0.value }
    self.systemUse = nt_HexBytes_0.value;

    return self;
  }

  
  return null;
}

async function VolumeDescriptorSetTerminator(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NULBytes_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\xffCD001\x01"[0, 7]
    left = 0;
    right = 7;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\xffCD001\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 7;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { descriptorType = "Volume Descriptor Set Terminator" }
    self.descriptorType = "Volume Descriptor Set Terminator";

    // NULBytes@0[7, EOI]
    left = 7;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_0 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_0 === null) break _ipg_alt;
    if (nt_NULBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_0._ipg_end);
    }
    nt_NULBytes_0._ipg_end += left;
    nt_NULBytes_0._ipg_start += left;
    left = nt_NULBytes_0._ipg_start;
    right = nt_NULBytes_0._ipg_end;

    return self;
  }

  
  return null;
}

async function UnknownVolumeDescriptor(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { typeByte = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.typeByte = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // "CD001"[1, 6]
    left = 1;
    right = 6;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "CD001")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 5;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { version = .[6] }
    left = 6;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.version = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { data = *[7, EOI] }
    left = 7;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.data = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { descriptorType = "Unknown Volume Descriptor" }
    self.descriptorType = "Unknown Volume Descriptor";

    return self;
  }

  
  return null;
}

async function DirectoriesRecursive(input, begin = 0, end = input.length, a_logicalBlockSize, a_node) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DirectoryRecords_0;
    let nt_DirectoriesRecursive_0;
    let seq_DirectoriesRecursive_0; let seq_DirectoriesRecursive_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ get(node, "isDirectory") ]
    if (!get(a_node, "isDirectory")) break _ipg_alt;

    // { offset = logicalBlockSize * get(node, "locationOfExtent") }
    self.offset = a_logicalBlockSize * get(a_node, "locationOfExtent");

    // { record = node }
    self.record = a_node;

    // DirectoryRecords@0[offset, offset + get(node, "dataLength")]
    left = self.offset;
    right = self.offset + get(a_node, "dataLength");
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DirectoryRecords_0 = await DirectoryRecords(input, begin + left, begin + right);
    if (nt_DirectoryRecords_0 === null) break _ipg_alt;
    if (nt_DirectoryRecords_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoryRecords_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoryRecords_0._ipg_end);
    }
    nt_DirectoryRecords_0._ipg_end += left;
    nt_DirectoryRecords_0._ipg_start += left;
    left = nt_DirectoryRecords_0._ipg_start;
    right = nt_DirectoryRecords_0._ipg_end;

    // for i = 2 to length(DirectoryRecords@0.values) do DirectoriesRecursive@0(logicalBlockSize, DirectoryRecords@0.values[i])[0, EOI]
    nt_DirectoriesRecursive_0 = { _ipg_end: right, _ipg_start: left };
    seq_DirectoriesRecursive_0_start = 2;
    loopEnd = length(nt_DirectoryRecords_0.values);
    seq_DirectoriesRecursive_0 = new Array(Math.max(0, loopEnd - seq_DirectoriesRecursive_0_start));
    for (let i_i = seq_DirectoriesRecursive_0_start; i_i < loopEnd; i_i++) {
      const left = 0;
      const right = EOI;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = await DirectoriesRecursive(input, begin + left, begin + right, a_logicalBlockSize, nt_DirectoryRecords_0.values[i_i]);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DirectoriesRecursive_0._ipg_end = tmp._ipg_end;
      nt_DirectoriesRecursive_0._ipg_start = tmp._ipg_start;
      seq_DirectoriesRecursive_0[i_i - seq_DirectoriesRecursive_0_start] = tmp;
    }
    left = nt_DirectoriesRecursive_0._ipg_start;
    right = nt_DirectoriesRecursive_0._ipg_end;

    // { children = DirectoriesRecursive@0.these }
    self.children = seq_DirectoriesRecursive_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { offset = logicalBlockSize * get(node, "locationOfExtent") }
    self.offset = a_logicalBlockSize * get(a_node, "locationOfExtent");

    // { record = node }
    self.record = a_node;

    return self;
  }

  
  return null;
}

async function DirectoryRecords(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DirectoryRecord_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat DirectoryRecord@0[DirectoryRecord@0.END, EOI].this starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_DirectoryRecord_0 = await DirectoryRecord(input, begin + left, begin + right);
    if (nt_DirectoryRecord_0 !== null) {
      if (nt_DirectoryRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: DirectoryRecord';
      self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoryRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoryRecord_0._ipg_end);
      nt_DirectoryRecord_0._ipg_end += left;
      nt_DirectoryRecord_0._ipg_start += left;
      left = nt_DirectoryRecord_0._ipg_end;
      right = EOI;
      self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_DirectoryRecord_0));

      while (left >= 0 && left <= right && right <= EOI) {
        nt_DirectoryRecord_0 = await DirectoryRecord(input, begin + left, begin + right);
        if (nt_DirectoryRecord_0 === null) break;
        if (nt_DirectoryRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: DirectoryRecord';
        self._ipg_start = Math.min(self._ipg_start, left + nt_DirectoryRecord_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_DirectoryRecord_0._ipg_end);
        nt_DirectoryRecord_0._ipg_end += left;
        nt_DirectoryRecord_0._ipg_start += left;
        self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_DirectoryRecord_0));
        left = nt_DirectoryRecord_0._ipg_end;
        right = EOI;
      }
    }

    return self;
  }

  
  return null;
}

async function DirectoryRecord(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BB_U32_0;
    let nt_BB_U32_1;
    let nt_RecordingDateAndTime_0;
    let nt_Byte_0;
    let nt_BB_U16_0;
    let nt_String_0;
    let nt_EvenPadByte_0;
    let nt_HexBytes_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { length = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.length = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // ?[ length > 0 ]
    if (!(self.length > 0)) break _ipg_alt;

    // { extendedAttributeRecordLength = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.extendedAttributeRecordLength = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BB_U32@0[2, EOI]
    left = 2;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_0 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_0 === null) break _ipg_alt;
    if (nt_BB_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_0._ipg_end);
    }
    nt_BB_U32_0._ipg_end += left;
    nt_BB_U32_0._ipg_start += left;
    left = nt_BB_U32_0._ipg_start;
    right = nt_BB_U32_0._ipg_end;

    // { locationOfExtent = BB_U32@0.value }
    self.locationOfExtent = nt_BB_U32_0.value;

    // BB_U32@1[BB_U32@0.END, EOI]
    left = nt_BB_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U32_1 = await BB_U32(input, begin + left, begin + right);
    if (nt_BB_U32_1 === null) break _ipg_alt;
    if (nt_BB_U32_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U32_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U32_1._ipg_end);
    }
    nt_BB_U32_1._ipg_end += left;
    nt_BB_U32_1._ipg_start += left;
    left = nt_BB_U32_1._ipg_start;
    right = nt_BB_U32_1._ipg_end;

    // { dataLength = BB_U32@1.value }
    self.dataLength = nt_BB_U32_1.value;

    // RecordingDateAndTime@0[BB_U32@1.END, BB_U32@1.END + 7]
    left = nt_BB_U32_1._ipg_end;
    right = nt_BB_U32_1._ipg_end + 7;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RecordingDateAndTime_0 = await RecordingDateAndTime(input, begin + left, begin + right);
    if (nt_RecordingDateAndTime_0 === null) break _ipg_alt;
    if (nt_RecordingDateAndTime_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RecordingDateAndTime_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RecordingDateAndTime_0._ipg_end);
    }
    nt_RecordingDateAndTime_0._ipg_end += left;
    nt_RecordingDateAndTime_0._ipg_start += left;
    left = nt_RecordingDateAndTime_0._ipg_start;
    right = nt_RecordingDateAndTime_0._ipg_end;

    // { recordingDateAndTime = RecordingDateAndTime@0.this }
    self.recordingDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_RecordingDateAndTime_0);

    // Byte@0[RecordingDateAndTime@0.END, EOI]
    left = nt_RecordingDateAndTime_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte_0 = await Byte(input, begin + left, begin + right);
    if (nt_Byte_0 === null) break _ipg_alt;
    if (nt_Byte_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte_0._ipg_end);
    }
    nt_Byte_0._ipg_end += left;
    nt_Byte_0._ipg_start += left;
    left = nt_Byte_0._ipg_start;
    right = nt_Byte_0._ipg_end;

    // { isHidden = (1 & Byte@0.value) != 0 }
    self.isHidden = (1 & nt_Byte_0.value) != 0;

    // { isDirectory = (2 & Byte@0.value) != 0 }
    self.isDirectory = (2 & nt_Byte_0.value) != 0;

    // { isAssociatedFile = (4 & Byte@0.value) != 0 }
    self.isAssociatedFile = (4 & nt_Byte_0.value) != 0;

    // { isRecord = (8 & Byte@0.value) != 0 }
    self.isRecord = (8 & nt_Byte_0.value) != 0;

    // { hasPermissions = (16 & Byte@0.value) != 0 }
    self.hasPermissions = (16 & nt_Byte_0.value) != 0;

    // { isMultiExtent = (128 & Byte@0.value) != 0 }
    self.isMultiExtent = (128 & nt_Byte_0.value) != 0;

    // { fileUnitSize = .[Byte@0.END] }
    left = nt_Byte_0._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.fileUnitSize = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { interleaveGapSize = .[Byte@0.END + 1] }
    left = nt_Byte_0._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.interleaveGapSize = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BB_U16@0[Byte@0.END + 2, EOI]
    left = nt_Byte_0._ipg_end + 2;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_0 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_0 === null) break _ipg_alt;
    if (nt_BB_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_0._ipg_end);
    }
    nt_BB_U16_0._ipg_end += left;
    nt_BB_U16_0._ipg_start += left;
    left = nt_BB_U16_0._ipg_start;
    right = nt_BB_U16_0._ipg_end;

    // { volumeSequenceNumber = BB_U16@0.value }
    self.volumeSequenceNumber = nt_BB_U16_0.value;

    // { lengthOfFileIdentifier = .[BB_U16@0.END] }
    left = nt_BB_U16_0._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lengthOfFileIdentifier = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // String@0[BB_U16@0.END + 1, BB_U16@0.END + 1 + lengthOfFileIdentifier]
    left = nt_BB_U16_0._ipg_end + 1;
    right = nt_BB_U16_0._ipg_end + 1 + self.lengthOfFileIdentifier;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = await String(input, begin + left, begin + right);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { fileIdentifier = String@0.value }
    self.fileIdentifier = nt_String_0.value;

    // EvenPadByte@0(lengthOfFileIdentifier)[String@0.END, EOI]
    left = nt_String_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_EvenPadByte_0 = await EvenPadByte(input, begin + left, begin + right, self.lengthOfFileIdentifier);
    if (nt_EvenPadByte_0 === null) break _ipg_alt;
    if (nt_EvenPadByte_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_EvenPadByte_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_EvenPadByte_0._ipg_end);
    }
    nt_EvenPadByte_0._ipg_end += left;
    nt_EvenPadByte_0._ipg_start += left;
    left = nt_EvenPadByte_0._ipg_start;
    right = nt_EvenPadByte_0._ipg_end;

    // HexBytes@0[EvenPadByte@0.END, length]
    left = nt_EvenPadByte_0._ipg_end;
    right = self.length;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { systemUse = HexBytes@0.value }
    self.systemUse = nt_HexBytes_0.value;

    return self;
  }

  
  return null;
}

async function RecordingDateAndTime(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Byte_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Byte@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Byte_0 = await Byte(input, begin + left, begin + right);
    if (nt_Byte_0 === null) break _ipg_alt;
    if (nt_Byte_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Byte_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Byte_0._ipg_end);
    }
    nt_Byte_0._ipg_end += left;
    nt_Byte_0._ipg_start += left;
    left = nt_Byte_0._ipg_start;
    right = nt_Byte_0._ipg_end;

    // { year = Byte@0.value + 1900 }
    self.year = nt_Byte_0.value + 1900;

    // { month = .[Byte@0.END] }
    left = nt_Byte_0._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.month = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { day = .[Byte@0.END + 1] }
    left = nt_Byte_0._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.day = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { hour = .[Byte@0.END + 2] }
    left = nt_Byte_0._ipg_end + 2;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.hour = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { minute = .[Byte@0.END + 3] }
    left = nt_Byte_0._ipg_end + 3;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.minute = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { second = .[Byte@0.END + 4] }
    left = nt_Byte_0._ipg_end + 4;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.second = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { gmtOffset = .[Byte@0.END + 5] }
    left = nt_Byte_0._ipg_end + 5;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.gmtOffset = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function LPathTableRecords(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LPathTableRecord_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat LPathTableRecord@0[LPathTableRecord@0.END, EOI].this starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_LPathTableRecord_0 = await LPathTableRecord(input, begin + left, begin + right);
    if (nt_LPathTableRecord_0 !== null) {
      if (nt_LPathTableRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: LPathTableRecord';
      self._ipg_start = Math.min(self._ipg_start, left + nt_LPathTableRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LPathTableRecord_0._ipg_end);
      nt_LPathTableRecord_0._ipg_end += left;
      nt_LPathTableRecord_0._ipg_start += left;
      left = nt_LPathTableRecord_0._ipg_end;
      right = EOI;
      self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_LPathTableRecord_0));

      while (left >= 0 && left <= right && right <= EOI) {
        nt_LPathTableRecord_0 = await LPathTableRecord(input, begin + left, begin + right);
        if (nt_LPathTableRecord_0 === null) break;
        if (nt_LPathTableRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: LPathTableRecord';
        self._ipg_start = Math.min(self._ipg_start, left + nt_LPathTableRecord_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_LPathTableRecord_0._ipg_end);
        nt_LPathTableRecord_0._ipg_end += left;
        nt_LPathTableRecord_0._ipg_start += left;
        self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_LPathTableRecord_0));
        left = nt_LPathTableRecord_0._ipg_end;
        right = EOI;
      }
    }

    return self;
  }

  
  return null;
}

async function LPathTableRecord(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LE_U32_0;
    let nt_LE_U16_0;
    let nt_String_0;
    let nt_OddPadByte_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { lengthOfDirectoryIdentifier = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lengthOfDirectoryIdentifier = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { extendedAttributeRecordLength = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.extendedAttributeRecordLength = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // LE_U32@0[2, EOI]
    left = 2;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_0 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_0 === null) break _ipg_alt;
    if (nt_LE_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_0._ipg_end);
    }
    nt_LE_U32_0._ipg_end += left;
    nt_LE_U32_0._ipg_start += left;
    left = nt_LE_U32_0._ipg_start;
    right = nt_LE_U32_0._ipg_end;

    // { locationOfExtent = LE_U32@0.value }
    self.locationOfExtent = nt_LE_U32_0.value;

    // LE_U16@0[LE_U32@0.END, EOI]
    left = nt_LE_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U16_0 = await LE_U16(input, begin + left, begin + right);
    if (nt_LE_U16_0 === null) break _ipg_alt;
    if (nt_LE_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U16_0._ipg_end);
    }
    nt_LE_U16_0._ipg_end += left;
    nt_LE_U16_0._ipg_start += left;
    left = nt_LE_U16_0._ipg_start;
    right = nt_LE_U16_0._ipg_end;

    // { parentDirectoryNumber = LE_U16@0.value }
    self.parentDirectoryNumber = nt_LE_U16_0.value;

    // String@0[LE_U16@0.END, LE_U16@0.END + lengthOfDirectoryIdentifier]
    left = nt_LE_U16_0._ipg_end;
    right = nt_LE_U16_0._ipg_end + self.lengthOfDirectoryIdentifier;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = await String(input, begin + left, begin + right);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { directoryIdentifier = String@0.value }
    self.directoryIdentifier = nt_String_0.value;

    // OddPadByte@0(lengthOfDirectoryIdentifier)[String@0.END, EOI]
    left = nt_String_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OddPadByte_0 = await OddPadByte(input, begin + left, begin + right, self.lengthOfDirectoryIdentifier);
    if (nt_OddPadByte_0 === null) break _ipg_alt;
    if (nt_OddPadByte_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OddPadByte_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OddPadByte_0._ipg_end);
    }
    nt_OddPadByte_0._ipg_end += left;
    nt_OddPadByte_0._ipg_start += left;
    left = nt_OddPadByte_0._ipg_start;
    right = nt_OddPadByte_0._ipg_end;

    return self;
  }

  
  return null;
}

async function MPathTableRecords(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_MPathTableRecord_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat MPathTableRecord@0[MPathTableRecord@0.END, EOI].this starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_MPathTableRecord_0 = await MPathTableRecord(input, begin + left, begin + right);
    if (nt_MPathTableRecord_0 !== null) {
      if (nt_MPathTableRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: MPathTableRecord';
      self._ipg_start = Math.min(self._ipg_start, left + nt_MPathTableRecord_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_MPathTableRecord_0._ipg_end);
      nt_MPathTableRecord_0._ipg_end += left;
      nt_MPathTableRecord_0._ipg_start += left;
      left = nt_MPathTableRecord_0._ipg_end;
      right = EOI;
      self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_MPathTableRecord_0));

      while (left >= 0 && left <= right && right <= EOI) {
        nt_MPathTableRecord_0 = await MPathTableRecord(input, begin + left, begin + right);
        if (nt_MPathTableRecord_0 === null) break;
        if (nt_MPathTableRecord_0._ipg_end === 0) throw 'repeat of non-consuming rule: MPathTableRecord';
        self._ipg_start = Math.min(self._ipg_start, left + nt_MPathTableRecord_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_MPathTableRecord_0._ipg_end);
        nt_MPathTableRecord_0._ipg_end += left;
        nt_MPathTableRecord_0._ipg_start += left;
        self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_MPathTableRecord_0));
        left = nt_MPathTableRecord_0._ipg_end;
        right = EOI;
      }
    }

    return self;
  }

  
  return null;
}

async function MPathTableRecord(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BE_U32_0;
    let nt_BE_U16_0;
    let nt_String_0;
    let nt_OddPadByte_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { lengthOfDirectoryIdentifier = .[0] }
    left = 0;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lengthOfDirectoryIdentifier = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { extendedAttributeRecordLength = .[1] }
    left = 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.extendedAttributeRecordLength = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BE_U32@0[2, EOI]
    left = 2;
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

    // { locationOfExtent = BE_U32@0.value }
    self.locationOfExtent = nt_BE_U32_0.value;

    // BE_U16@0[BE_U32@0.END, EOI]
    left = nt_BE_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U16_0 = await BE_U16(input, begin + left, begin + right);
    if (nt_BE_U16_0 === null) break _ipg_alt;
    if (nt_BE_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U16_0._ipg_end);
    }
    nt_BE_U16_0._ipg_end += left;
    nt_BE_U16_0._ipg_start += left;
    left = nt_BE_U16_0._ipg_start;
    right = nt_BE_U16_0._ipg_end;

    // { parentDirectoryNumber = BE_U16@0.value }
    self.parentDirectoryNumber = nt_BE_U16_0.value;

    // String@0[BE_U16@0.END, BE_U16@0.END + lengthOfDirectoryIdentifier]
    left = nt_BE_U16_0._ipg_end;
    right = nt_BE_U16_0._ipg_end + self.lengthOfDirectoryIdentifier;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = await String(input, begin + left, begin + right);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { directoryIdentifier = String@0.value }
    self.directoryIdentifier = nt_String_0.value;

    // OddPadByte@0(lengthOfDirectoryIdentifier)[String@0.END, EOI]
    left = nt_String_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_OddPadByte_0 = await OddPadByte(input, begin + left, begin + right, self.lengthOfDirectoryIdentifier);
    if (nt_OddPadByte_0 === null) break _ipg_alt;
    if (nt_OddPadByte_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_OddPadByte_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_OddPadByte_0._ipg_end);
    }
    nt_OddPadByte_0._ipg_end += left;
    nt_OddPadByte_0._ipg_start += left;
    left = nt_OddPadByte_0._ipg_start;
    right = nt_OddPadByte_0._ipg_end;

    return self;
  }

  
  return null;
}

async function ExtendedAttributeRecord(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_BB_U16_0;
    let nt_BB_U16_1;
    let nt_BE_U16_0;
    let nt_DateAndTime_0;
    let nt_DateAndTime_1;
    let nt_DateAndTime_2;
    let nt_DateAndTime_3;
    let nt_BB_U16_2;
    let nt_String_0;
    let nt_HexBytes_0;
    let nt_NULBytes_0;
    let nt_BB_U16_3;
    let nt_HexBytes_1;
    let nt_HexBytes_2;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // BB_U16@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_0 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_0 === null) break _ipg_alt;
    if (nt_BB_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_0._ipg_end);
    }
    nt_BB_U16_0._ipg_end += left;
    nt_BB_U16_0._ipg_start += left;
    left = nt_BB_U16_0._ipg_start;
    right = nt_BB_U16_0._ipg_end;

    // { ownerIdentification = BB_U16@0.value }
    self.ownerIdentification = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, EOI]
    left = nt_BB_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_1 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_1 === null) break _ipg_alt;
    if (nt_BB_U16_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_1._ipg_end);
    }
    nt_BB_U16_1._ipg_end += left;
    nt_BB_U16_1._ipg_start += left;
    left = nt_BB_U16_1._ipg_start;
    right = nt_BB_U16_1._ipg_end;

    // { groupIdentification = BB_U16@1.value }
    self.groupIdentification = nt_BB_U16_1.value;

    // BE_U16@0[BB_U16@1.END, EOI]
    left = nt_BB_U16_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U16_0 = await BE_U16(input, begin + left, begin + right);
    if (nt_BE_U16_0 === null) break _ipg_alt;
    if (nt_BE_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U16_0._ipg_end);
    }
    nt_BE_U16_0._ipg_end += left;
    nt_BE_U16_0._ipg_start += left;
    left = nt_BE_U16_0._ipg_start;
    right = nt_BE_U16_0._ipg_end;

    // { permissions = BE_U16@0.value }
    self.permissions = nt_BE_U16_0.value;

    // DateAndTime@0[BE_U16@0.END, EOI]
    left = nt_BE_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_0 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_0 === null) break _ipg_alt;
    if (nt_DateAndTime_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_0._ipg_end);
    }
    nt_DateAndTime_0._ipg_end += left;
    nt_DateAndTime_0._ipg_start += left;
    left = nt_DateAndTime_0._ipg_start;
    right = nt_DateAndTime_0._ipg_end;

    // { fileCreationDateAndTime = DateAndTime@0.this }
    self.fileCreationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_0);

    // DateAndTime@1[DateAndTime@0.END, EOI]
    left = nt_DateAndTime_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_1 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_1 === null) break _ipg_alt;
    if (nt_DateAndTime_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_1._ipg_end);
    }
    nt_DateAndTime_1._ipg_end += left;
    nt_DateAndTime_1._ipg_start += left;
    left = nt_DateAndTime_1._ipg_start;
    right = nt_DateAndTime_1._ipg_end;

    // { fileModificationDateAndTime = DateAndTime@1.this }
    self.fileModificationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_1);

    // DateAndTime@2[DateAndTime@1.END, EOI]
    left = nt_DateAndTime_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_2 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_2 === null) break _ipg_alt;
    if (nt_DateAndTime_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_2._ipg_end);
    }
    nt_DateAndTime_2._ipg_end += left;
    nt_DateAndTime_2._ipg_start += left;
    left = nt_DateAndTime_2._ipg_start;
    right = nt_DateAndTime_2._ipg_end;

    // { fileExpirationDateAndTime = DateAndTime@2.this }
    self.fileExpirationDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_2);

    // DateAndTime@3[DateAndTime@2.END, EOI]
    left = nt_DateAndTime_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DateAndTime_3 = await DateAndTime(input, begin + left, begin + right);
    if (nt_DateAndTime_3 === null) break _ipg_alt;
    if (nt_DateAndTime_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DateAndTime_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DateAndTime_3._ipg_end);
    }
    nt_DateAndTime_3._ipg_end += left;
    nt_DateAndTime_3._ipg_start += left;
    left = nt_DateAndTime_3._ipg_start;
    right = nt_DateAndTime_3._ipg_end;

    // { fileEffectiveDateAndTime = DateAndTime@3.this }
    self.fileEffectiveDateAndTime = (({_ipg_start,_ipg_end,...o}) => o)(nt_DateAndTime_3);

    // { recordFormat = .[DateAndTime@3.END] }
    left = nt_DateAndTime_3._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.recordFormat = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { recordAttributes = .[DateAndTime@3.END + 1] }
    left = nt_DateAndTime_3._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.recordAttributes = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // BB_U16@2[DateAndTime@3.END + 2, EOI]
    left = nt_DateAndTime_3._ipg_end + 2;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_2 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_2 === null) break _ipg_alt;
    if (nt_BB_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_2._ipg_end);
    }
    nt_BB_U16_2._ipg_end += left;
    nt_BB_U16_2._ipg_start += left;
    left = nt_BB_U16_2._ipg_start;
    right = nt_BB_U16_2._ipg_end;

    // { recordLength = BB_U16@2.value }
    self.recordLength = nt_BB_U16_2.value;

    // String@0[BB_U16@2.END, BB_U16@2.END + 32]
    left = nt_BB_U16_2._ipg_end;
    right = nt_BB_U16_2._ipg_end + 32;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_String_0 = await String(input, begin + left, begin + right);
    if (nt_String_0 === null) break _ipg_alt;
    if (nt_String_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_String_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_String_0._ipg_end);
    }
    nt_String_0._ipg_end += left;
    nt_String_0._ipg_start += left;
    left = nt_String_0._ipg_start;
    right = nt_String_0._ipg_end;

    // { systemIdentifier = String@0.value }
    self.systemIdentifier = nt_String_0.value;

    // HexBytes@0[String@0.END, String@0.END + 64]
    left = nt_String_0._ipg_end;
    right = nt_String_0._ipg_end + 64;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_0 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_0 === null) break _ipg_alt;
    if (nt_HexBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_0._ipg_end);
    }
    nt_HexBytes_0._ipg_end += left;
    nt_HexBytes_0._ipg_start += left;
    left = nt_HexBytes_0._ipg_start;
    right = nt_HexBytes_0._ipg_end;

    // { systemUse = HexBytes@0.value }
    self.systemUse = nt_HexBytes_0.value;

    // "\x01"[HexBytes@0.END, HexBytes@0.END + 1]
    left = nt_HexBytes_0._ipg_end;
    right = nt_HexBytes_0._ipg_end + 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!await _ipg_startsWith(input, begin + left, begin + right, "\x01")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

    // { lengthOfEscapeSequences = .[HexBytes@0.END + 1] }
    left = nt_HexBytes_0._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.lengthOfEscapeSequences = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // NULBytes@0[HexBytes@0.END + 2, HexBytes@0.END + 66]
    left = nt_HexBytes_0._ipg_end + 2;
    right = nt_HexBytes_0._ipg_end + 66;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NULBytes_0 = await NULBytes(input, begin + left, begin + right);
    if (nt_NULBytes_0 === null) break _ipg_alt;
    if (nt_NULBytes_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NULBytes_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NULBytes_0._ipg_end);
    }
    nt_NULBytes_0._ipg_end += left;
    nt_NULBytes_0._ipg_start += left;
    left = nt_NULBytes_0._ipg_start;
    right = nt_NULBytes_0._ipg_end;

    // BB_U16@3[NULBytes@0.END, EOI]
    left = nt_NULBytes_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BB_U16_3 = await BB_U16(input, begin + left, begin + right);
    if (nt_BB_U16_3 === null) break _ipg_alt;
    if (nt_BB_U16_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BB_U16_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BB_U16_3._ipg_end);
    }
    nt_BB_U16_3._ipg_end += left;
    nt_BB_U16_3._ipg_start += left;
    left = nt_BB_U16_3._ipg_start;
    right = nt_BB_U16_3._ipg_end;

    // { lengthOfApplicationUse = BB_U16@3.value }
    self.lengthOfApplicationUse = nt_BB_U16_3.value;

    // HexBytes@1[BB_U16@3.END, BB_U16@3.END + lengthOfApplicationUse]
    left = nt_BB_U16_3._ipg_end;
    right = nt_BB_U16_3._ipg_end + self.lengthOfApplicationUse;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_1 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_1 === null) break _ipg_alt;
    if (nt_HexBytes_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_1._ipg_end);
    }
    nt_HexBytes_1._ipg_end += left;
    nt_HexBytes_1._ipg_start += left;
    left = nt_HexBytes_1._ipg_start;
    right = nt_HexBytes_1._ipg_end;

    // { applicationUse = HexBytes@1.value }
    self.applicationUse = nt_HexBytes_1.value;

    // HexBytes@2[HexBytes@1.END, HexBytes@1.END + lengthOfEscapeSequences]
    left = nt_HexBytes_1._ipg_end;
    right = nt_HexBytes_1._ipg_end + self.lengthOfEscapeSequences;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_HexBytes_2 = await HexBytes(input, begin + left, begin + right);
    if (nt_HexBytes_2 === null) break _ipg_alt;
    if (nt_HexBytes_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_HexBytes_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_HexBytes_2._ipg_end);
    }
    nt_HexBytes_2._ipg_end += left;
    nt_HexBytes_2._ipg_start += left;
    left = nt_HexBytes_2._ipg_start;
    right = nt_HexBytes_2._ipg_end;

    // { escapeSequences = HexBytes@2.value }
    self.escapeSequences = nt_HexBytes_2.value;

    return self;
  }

  
  return null;
}

async function EvenPadByte(input, begin = 0, end = input.length, a_n) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ n % 2 == 0 ]
    if (!(a_n % 2 == 0)) break _ipg_alt;

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
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ""[0, 0]
    left = 0;
    right = 0;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;

    return self;
  }

  
  return null;
}

async function OddPadByte(input, begin = 0, end = input.length, a_n) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ n % 2 == 1 ]
    if (!(a_n % 2 == 1)) break _ipg_alt;

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
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ""[0, 0]
    left = 0;
    right = 0;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;

    return self;
  }

  
  return null;
}

async function String(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bytes = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bytes = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = decodeAscii(bytes) }
    self.value = decodeAscii(self.bytes);

    return self;
  }

  
  return null;
}

async function AChars(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bytes = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bytes = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = decodeAscii(bytes) }
    self.value = decodeAscii(self.bytes);

    return self;
  }

  
  return null;
}

async function DChars(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bytes = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bytes = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = decodeAscii(bytes) }
    self.value = decodeAscii(self.bytes);

    return self;
  }

  
  return null;
}

async function DateAndTime(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Digits_0;
    let nt_Digits_1;
    let nt_Digits_2;
    let nt_Digits_3;
    let nt_Digits_4;
    let nt_Digits_5;
    let nt_Digits_6;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // Digits@0[0, 4]
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_0 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_0 === null) break _ipg_alt;
    if (nt_Digits_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_0._ipg_end);
    }
    nt_Digits_0._ipg_end += left;
    nt_Digits_0._ipg_start += left;
    left = nt_Digits_0._ipg_start;
    right = nt_Digits_0._ipg_end;

    // { year = Digits@0.value }
    self.year = nt_Digits_0.value;

    // Digits@1[Digits@0.END, Digits@0.END + 2]
    left = nt_Digits_0._ipg_end;
    right = nt_Digits_0._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_1 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_1 === null) break _ipg_alt;
    if (nt_Digits_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_1._ipg_end);
    }
    nt_Digits_1._ipg_end += left;
    nt_Digits_1._ipg_start += left;
    left = nt_Digits_1._ipg_start;
    right = nt_Digits_1._ipg_end;

    // { month = Digits@1.value }
    self.month = nt_Digits_1.value;

    // Digits@2[Digits@1.END, Digits@1.END + 2]
    left = nt_Digits_1._ipg_end;
    right = nt_Digits_1._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_2 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_2 === null) break _ipg_alt;
    if (nt_Digits_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_2._ipg_end);
    }
    nt_Digits_2._ipg_end += left;
    nt_Digits_2._ipg_start += left;
    left = nt_Digits_2._ipg_start;
    right = nt_Digits_2._ipg_end;

    // { day = Digits@2.value }
    self.day = nt_Digits_2.value;

    // Digits@3[Digits@2.END, Digits@2.END + 2]
    left = nt_Digits_2._ipg_end;
    right = nt_Digits_2._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_3 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_3 === null) break _ipg_alt;
    if (nt_Digits_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_3._ipg_end);
    }
    nt_Digits_3._ipg_end += left;
    nt_Digits_3._ipg_start += left;
    left = nt_Digits_3._ipg_start;
    right = nt_Digits_3._ipg_end;

    // { hour = Digits@3.value }
    self.hour = nt_Digits_3.value;

    // Digits@4[Digits@3.END, Digits@3.END + 2]
    left = nt_Digits_3._ipg_end;
    right = nt_Digits_3._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_4 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_4 === null) break _ipg_alt;
    if (nt_Digits_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_4._ipg_end);
    }
    nt_Digits_4._ipg_end += left;
    nt_Digits_4._ipg_start += left;
    left = nt_Digits_4._ipg_start;
    right = nt_Digits_4._ipg_end;

    // { minute = Digits@4.value }
    self.minute = nt_Digits_4.value;

    // Digits@5[Digits@4.END, Digits@4.END + 2]
    left = nt_Digits_4._ipg_end;
    right = nt_Digits_4._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_5 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_5 === null) break _ipg_alt;
    if (nt_Digits_5._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_5._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_5._ipg_end);
    }
    nt_Digits_5._ipg_end += left;
    nt_Digits_5._ipg_start += left;
    left = nt_Digits_5._ipg_start;
    right = nt_Digits_5._ipg_end;

    // { second = Digits@5.value }
    self.second = nt_Digits_5.value;

    // Digits@6[Digits@5.END, Digits@5.END + 2]
    left = nt_Digits_5._ipg_end;
    right = nt_Digits_5._ipg_end + 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Digits_6 = await Digits(input, begin + left, begin + right);
    if (nt_Digits_6 === null) break _ipg_alt;
    if (nt_Digits_6._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digits_6._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digits_6._ipg_end);
    }
    nt_Digits_6._ipg_end += left;
    nt_Digits_6._ipg_start += left;
    left = nt_Digits_6._ipg_start;
    right = nt_Digits_6._ipg_end;

    // { hundrethsOfSecond = Digits@6.value }
    self.hundrethsOfSecond = nt_Digits_6.value;

    // { gmtOffset = .[Digits@6.END] }
    left = nt_Digits_6._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.gmtOffset = await input.at(begin + left);
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    return self;
  }

  
  return null;
}

async function Digits(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Digit_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Digit@0[Digit@0.END, EOI].value starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_Digit_0 = await Digit(input, begin + left, begin + right);
    if (nt_Digit_0 !== null) {
      if (nt_Digit_0._ipg_end === 0) throw 'repeat of non-consuming rule: Digit';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Digit_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Digit_0._ipg_end);
      nt_Digit_0._ipg_end += left;
      nt_Digit_0._ipg_start += left;
      left = nt_Digit_0._ipg_end;
      right = EOI;
      self.values.push(nt_Digit_0.value);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_Digit_0 = await Digit(input, begin + left, begin + right);
        if (nt_Digit_0 === null) break;
        if (nt_Digit_0._ipg_end === 0) throw 'repeat of non-consuming rule: Digit';
        self._ipg_start = Math.min(self._ipg_start, left + nt_Digit_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_Digit_0._ipg_end);
        nt_Digit_0._ipg_end += left;
        nt_Digit_0._ipg_start += left;
        self.values.push(nt_Digit_0.value);
        left = nt_Digit_0._ipg_end;
        right = EOI;
      }
    }

    // { value = decodeAscii2(values) }
    self.value = decodeAscii2(self.values);

    return self;
  }

  
  return null;
}

async function Digit(input, begin = 0, end = input.length) {
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

    // ?[ value == 0 || value >= 48 && value <= 57 ]
    if (!(self.value == 0 || self.value >= 48 && self.value <= 57)) break _ipg_alt;

    return self;
  }

  
  return null;
}

async function HexBytes(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { bytes = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.bytes = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = asHex(bytes) }
    self.value = asHex(self.bytes);

    return self;
  }

  
  return null;
}

async function LE_U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { b = *[0, 2] }
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.b = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = b[1] << 8 | b[0] }
    self.value = self.b[1] << 8 | self.b[0];

    return self;
  }

  
  return null;
}

async function BE_U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { b = *[0, 2] }
    left = 0;
    right = 2;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.b = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = b[0] << 8 | b[1] }
    self.value = self.b[0] << 8 | self.b[1];

    return self;
  }

  
  return null;
}

async function BB_U16(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LE_U16_0;
    let nt_BE_U16_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LE_U16@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U16_0 = await LE_U16(input, begin + left, begin + right);
    if (nt_LE_U16_0 === null) break _ipg_alt;
    if (nt_LE_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U16_0._ipg_end);
    }
    nt_LE_U16_0._ipg_end += left;
    nt_LE_U16_0._ipg_start += left;
    left = nt_LE_U16_0._ipg_start;
    right = nt_LE_U16_0._ipg_end;

    // BE_U16@0[LE_U16@0.END, EOI]
    left = nt_LE_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_BE_U16_0 = await BE_U16(input, begin + left, begin + right);
    if (nt_BE_U16_0 === null) break _ipg_alt;
    if (nt_BE_U16_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_BE_U16_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_BE_U16_0._ipg_end);
    }
    nt_BE_U16_0._ipg_end += left;
    nt_BE_U16_0._ipg_start += left;
    left = nt_BE_U16_0._ipg_start;
    right = nt_BE_U16_0._ipg_end;

    // ?[ LE_U16@0.value == BE_U16@0.value ]
    if (!(nt_LE_U16_0.value == nt_BE_U16_0.value)) break _ipg_alt;

    // { value = LE_U16@0.value }
    self.value = nt_LE_U16_0.value;

    return self;
  }

  
  return null;
}

async function LE_U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { b = *[0, 4] }
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.b = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = b[3] << 24 | b[2] << 16 | b[1] << 8 | b[0] }
    self.value = self.b[3] << 24 | self.b[2] << 16 | self.b[1] << 8 | self.b[0];

    return self;
  }

  
  return null;
}

async function BE_U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { b = *[0, 4] }
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.b = await input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // { value = b[0] << 24 | b[1] << 16 | b[2] << 8 | b[3] }
    self.value = self.b[0] << 24 | self.b[1] << 16 | self.b[2] << 8 | self.b[3];

    return self;
  }

  
  return null;
}

async function BB_U32(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_LE_U32_0;
    let nt_BE_U32_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // LE_U32@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_LE_U32_0 = await LE_U32(input, begin + left, begin + right);
    if (nt_LE_U32_0 === null) break _ipg_alt;
    if (nt_LE_U32_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_LE_U32_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_LE_U32_0._ipg_end);
    }
    nt_LE_U32_0._ipg_end += left;
    nt_LE_U32_0._ipg_start += left;
    left = nt_LE_U32_0._ipg_start;
    right = nt_LE_U32_0._ipg_end;

    // BE_U32@0[LE_U32@0.END, EOI]
    left = nt_LE_U32_0._ipg_end;
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

    // ?[ LE_U32@0.value == BE_U32@0.value ]
    if (!(nt_LE_U32_0.value == nt_BE_U32_0.value)) break _ipg_alt;

    // { value = LE_U32@0.value }
    self.value = nt_LE_U32_0.value;

    return self;
  }

  
  return null;
}

async function NULBytes(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NUL_BYTE_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat NUL_BYTE@0[NUL_BYTE@0.END, EOI].this starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_NUL_BYTE_0 = await NUL_BYTE(input, begin + left, begin + right);
    if (nt_NUL_BYTE_0 !== null) {
      if (nt_NUL_BYTE_0._ipg_end === 0) throw 'repeat of non-consuming rule: NUL_BYTE';
      self._ipg_start = Math.min(self._ipg_start, left + nt_NUL_BYTE_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NUL_BYTE_0._ipg_end);
      nt_NUL_BYTE_0._ipg_end += left;
      nt_NUL_BYTE_0._ipg_start += left;
      left = nt_NUL_BYTE_0._ipg_end;
      right = EOI;
      self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_NUL_BYTE_0));

      while (left >= 0 && left <= right && right <= EOI) {
        nt_NUL_BYTE_0 = await NUL_BYTE(input, begin + left, begin + right);
        if (nt_NUL_BYTE_0 === null) break;
        if (nt_NUL_BYTE_0._ipg_end === 0) throw 'repeat of non-consuming rule: NUL_BYTE';
        self._ipg_start = Math.min(self._ipg_start, left + nt_NUL_BYTE_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_NUL_BYTE_0._ipg_end);
        nt_NUL_BYTE_0._ipg_end += left;
        nt_NUL_BYTE_0._ipg_start += left;
        self.values.push((({_ipg_start,_ipg_end,...o}) => o)(nt_NUL_BYTE_0));
        left = nt_NUL_BYTE_0._ipg_end;
        right = EOI;
      }
    }

    return self;
  }

  
  return null;
}

async function NUL_BYTE(input, begin = 0, end = input.length) {
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

async function Byte(input, begin = 0, end = input.length) {
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
