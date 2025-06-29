const fs = require("node:fs");

function decodeAscii(bytes) {
  return new TextDecoder("ascii").decode(new Uint8Array(bytes));
}

function makeEntry(name, descriptor, type) {
  return {
    name: decodeAscii(name),
    descriptor: new Uint8Array(descriptor).values().toArray(),
    type,
  };
}

function empty() {
  return {};
}

function projectSections(sections) {
  return sections.map(o => o.section);
}
function _ipg_startsWith(s, l, r, prefix) {
  if (r - l < prefix.length) return false;
  if (typeof s === 'string') return s.startsWith(prefix, l);
  for (let i = 0; i < prefix.length; ++i) {
    if (s[l + i] !== prefix.charCodeAt(i)) return false;
  }
  return true;
}
function ELF(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_H;
    let nt_SH;
    let nt_Sec;
    let seq_SH; let seq_SH_start = 0;
    let seq_Sec; let seq_Sec_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // H[0, 128]
    left = 0;
    right = 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_H = H(input, begin + left, begin + right);
    if (nt_H === null) break _ipg_alt;
    if (nt_H._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_H._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_H._ipg_end);
    }
    nt_H._ipg_end += left;
    nt_H._ipg_start += left;
    left = nt_H._ipg_start;
    right = nt_H._ipg_end;

    // for i = 0 to H.e_shnum do SH[H.e_shoff + i * H.e_shentsize, H.e_shoff + (i + 1) * H.e_shentsize]
    nt_SH = { _ipg_end: right, _ipg_start: left };
    seq_SH_start = 0;
    loopEnd = nt_H.e_shnum;
    seq_SH = new Array(loopEnd - seq_SH_start);
    for (self.i = seq_SH_start; self.i < loopEnd; self.i++) {
      const left = nt_H.e_shoff + self.i * nt_H.e_shentsize;
      const right = nt_H.e_shoff + (self.i + 1) * nt_H.e_shentsize;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = SH(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_SH._ipg_end = tmp._ipg_end;
      nt_SH._ipg_start = tmp._ipg_start;
      seq_SH[self.i - seq_SH_start] = tmp;
    }
    delete self.i;
    left = nt_SH._ipg_start;
    right = nt_SH._ipg_end;

    // for i = 1 to H.e_shnum do Sec(SH(i).sh_type)[SH(i).sh_offset, SH(i).sh_offset + SH(i).sh_size]
    nt_Sec = { _ipg_end: right, _ipg_start: left };
    seq_Sec_start = 1;
    loopEnd = nt_H.e_shnum;
    seq_Sec = new Array(loopEnd - seq_Sec_start);
    for (self.i = seq_Sec_start; self.i < loopEnd; self.i++) {
      const left = seq_SH[self.i - seq_SH_start].sh_offset;
      const right = seq_SH[self.i - seq_SH_start].sh_offset + seq_SH[self.i - seq_SH_start].sh_size;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Sec(input, begin + left, begin + right, seq_SH[self.i - seq_SH_start].sh_type);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Sec._ipg_end = tmp._ipg_end;
      nt_Sec._ipg_start = tmp._ipg_start;
      seq_Sec[self.i - seq_Sec_start] = tmp;
    }
    delete self.i;
    left = nt_Sec._ipg_start;
    right = nt_Sec._ipg_end;

    // { header = H.this }
    self.header = (({_ipg_start,_ipg_end,...o}) => o)(nt_H);

    // { section_headers = SH.these }
    self.section_headers = seq_SH.map(({_ipg_start,_ipg_end,...o}) => o);

    // { sections = projectSections(Sec.these) }
    self.sections = projectSections(seq_Sec.map(({_ipg_start,_ipg_end,...o}) => o));

    return self;
  }

  
  return null;
}

function Sec(input, begin = 0, end = input.length, a_sh_type) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 6 ]
    if (!(a_sh_type == 6)) break _ipg_alt;

    // DynSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DynSec = DynSec(input, begin + left, begin + right);
    if (nt_DynSec === null) break _ipg_alt;
    if (nt_DynSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DynSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DynSec._ipg_end);
    }
    nt_DynSec._ipg_end += left;
    nt_DynSec._ipg_start += left;
    left = nt_DynSec._ipg_start;
    right = nt_DynSec._ipg_end;

    // { section = DynSec.section }
    self.section = nt_DynSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_StrSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 3 ]
    if (!(a_sh_type == 3)) break _ipg_alt;

    // StrSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_StrSec = StrSec(input, begin + left, begin + right);
    if (nt_StrSec === null) break _ipg_alt;
    if (nt_StrSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_StrSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_StrSec._ipg_end);
    }
    nt_StrSec._ipg_end += left;
    nt_StrSec._ipg_start += left;
    left = nt_StrSec._ipg_start;
    right = nt_StrSec._ipg_end;

    // { section = StrSec.section }
    self.section = nt_StrSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSymSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 11 || sh_type == 2 ]
    if (!(a_sh_type == 11 || a_sh_type == 2)) break _ipg_alt;

    // DynSymSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DynSymSec = DynSymSec(input, begin + left, begin + right);
    if (nt_DynSymSec === null) break _ipg_alt;
    if (nt_DynSymSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DynSymSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DynSymSec._ipg_end);
    }
    nt_DynSymSec._ipg_end += left;
    nt_DynSymSec._ipg_start += left;
    left = nt_DynSymSec._ipg_start;
    right = nt_DynSymSec._ipg_end;

    // { section = DynSymSec.section }
    self.section = nt_DynSymSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NoteSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 7 ]
    if (!(a_sh_type == 7)) break _ipg_alt;

    // NoteSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NoteSec = NoteSec(input, begin + left, begin + right);
    if (nt_NoteSec === null) break _ipg_alt;
    if (nt_NoteSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSec._ipg_end);
    }
    nt_NoteSec._ipg_end += left;
    nt_NoteSec._ipg_start += left;
    left = nt_NoteSec._ipg_start;
    right = nt_NoteSec._ipg_end;

    // { section = NoteSec.section }
    self.section = nt_NoteSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 9 ]
    if (!(a_sh_type == 9)) break _ipg_alt;

    // RelSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RelSec = RelSec(input, begin + left, begin + right);
    if (nt_RelSec === null) break _ipg_alt;
    if (nt_RelSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RelSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RelSec._ipg_end);
    }
    nt_RelSec._ipg_end += left;
    nt_RelSec._ipg_start += left;
    left = nt_RelSec._ipg_start;
    right = nt_RelSec._ipg_end;

    // { section = RelSec.section }
    self.section = nt_RelSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelAddEndSec;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 4 ]
    if (!(a_sh_type == 4)) break _ipg_alt;

    // RelAddEndSec[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RelAddEndSec = RelAddEndSec(input, begin + left, begin + right);
    if (nt_RelAddEndSec === null) break _ipg_alt;
    if (nt_RelAddEndSec._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RelAddEndSec._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RelAddEndSec._ipg_end);
    }
    nt_RelAddEndSec._ipg_end += left;
    nt_RelAddEndSec._ipg_start += left;
    left = nt_RelAddEndSec._ipg_start;
    right = nt_RelAddEndSec._ipg_end;

    // { section = RelAddEndSec.section }
    self.section = nt_RelAddEndSec.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 8 ]
    if (!(a_sh_type == 8)) break _ipg_alt;

    // { section = empty() }
    self.section = empty();

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { section = *[0, EOI] }
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.section = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    return self;
  }

  
  return null;
}

function NUL_BYTE(input, begin = 0, end = input.length) {
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

function U64(input, begin = 0, end = input.length) {
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

    // { value = bs[0] | bs[1] << 8 | bs[2] << 16 | bs[3] << 24 | bs[4] << 32 | bs[5] << 40 | bs[6] << 48 | bs[7] << 56 }
    self.value = self.bs[0] | self.bs[1] << 8 | self.bs[2] << 16 | self.bs[3] << 24 | self.bs[4] << 32 | self.bs[5] << 40 | self.bs[6] << 48 | self.bs[7] << 56;

    return self;
  }

  
  return null;
}

function H(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U16;
    let nt_U32;
    let nt_U64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x7fELF"[0, 4]
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x7fELF")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 4;

    // { ei_class = .[4] }
    left = 4;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.ei_class = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { ei_data = .[5] }
    left = 5;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.ei_data = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { ei_version = .[6] }
    left = 6;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.ei_version = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { abi = .[7] }
    left = 7;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.abi = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { abi_version = .[8] }
    left = 8;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.abi_version = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // U16[16, 18]
    left = 16;
    right = 18;
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

    // { e_type = U16.value }
    self.e_type = nt_U16.value;

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

    // { e_machine = U16.value }
    self.e_machine = nt_U16.value;

    // U32[U16.END, EOI]
    left = nt_U16._ipg_end;
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

    // { e_version = U32.value }
    self.e_version = nt_U32.value;

    // U64[U32.END, EOI]
    left = nt_U32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { e_entry = U64.value }
    self.e_entry = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { e_phoff = U64.value }
    self.e_phoff = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { e_shoff = U64.value }
    self.e_shoff = nt_U64.value;

    // U32[U64.END, EOI]
    left = nt_U64._ipg_end;
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

    // { e_flags = U32.value }
    self.e_flags = nt_U32.value;

    // U16[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { e_ehsize = U16.value }
    self.e_ehsize = nt_U16.value;

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

    // { e_phentsize = U16.value }
    self.e_phentsize = nt_U16.value;

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

    // { e_phnum = U16.value }
    self.e_phnum = nt_U16.value;

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

    // { e_shentsize = U16.value }
    self.e_shentsize = nt_U16.value;

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

    // { e_shnum = U16.value }
    self.e_shnum = nt_U16.value;

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

    // { e_shstrndx = U16.value }
    self.e_shstrndx = nt_U16.value;

    return self;
  }

  
  return null;
}

function SH(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32;
    let nt_U64;
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

    // { sh_name = U32.value }
    self.sh_name = nt_U32.value;

    // U32[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { sh_type = U32.value }
    self.sh_type = nt_U32.value;

    // U64[U32.END, EOI]
    left = nt_U32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { sh_flags = U64.value }
    self.sh_flags = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { sh_addr = U64.value }
    self.sh_addr = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { sh_offset = U64.value }
    self.sh_offset = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { sh_size = U64.value }
    self.sh_size = nt_U64.value;

    // U32[U64.END, EOI]
    left = nt_U64._ipg_end;
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

    // { sh_link = U32.value }
    self.sh_link = nt_U32.value;

    // U32[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { sh_info = U32.value }
    self.sh_info = nt_U32.value;

    // U32[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { sh_addralign = U32.value }
    self.sh_addralign = nt_U32.value;

    // U64[U32.END, EOI]
    left = nt_U32._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { sh_entsize = U64.value }
    self.sh_entsize = nt_U64.value;

    return self;
  }

  
  return null;
}

function DynSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSecEntry;
    let seq_DynSecEntry; let seq_DynSecEntry_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 16 do DynSecEntry[16 * i, 16 * (i + 1)]
    nt_DynSecEntry = { _ipg_end: right, _ipg_start: left };
    seq_DynSecEntry_start = 0;
    loopEnd = EOI / 16;
    seq_DynSecEntry = new Array(loopEnd - seq_DynSecEntry_start);
    for (self.i = seq_DynSecEntry_start; self.i < loopEnd; self.i++) {
      const left = 16 * self.i;
      const right = 16 * (self.i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = DynSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DynSecEntry._ipg_end = tmp._ipg_end;
      nt_DynSecEntry._ipg_start = tmp._ipg_start;
      seq_DynSecEntry[self.i - seq_DynSecEntry_start] = tmp;
    }
    delete self.i;
    left = nt_DynSecEntry._ipg_start;
    right = nt_DynSecEntry._ipg_end;

    // { section = DynSecEntry.these }
    self.section = seq_DynSecEntry.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function DynSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { tag = U64.value }
    self.tag = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { value_or_ptr = U64.value }
    self.value_or_ptr = nt_U64.value;

    return self;
  }

  
  return null;
}

function StrSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Str;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Str[Str.END, EOI].string starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_Str = Str(input, begin + left, begin + right);
    if (nt_Str !== null) {
      if (nt_Str._ipg_end === 0) throw 'repeat of non-consuming rule: Str';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Str._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Str._ipg_end);
      nt_Str._ipg_end += left;
      nt_Str._ipg_start += left;
      left = nt_Str._ipg_end;
      right = EOI;
      self.values.push(nt_Str.string);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_Str = Str(input, begin + left, begin + right);
        if (nt_Str === null) break;
        if (nt_Str._ipg_end === 0) throw 'repeat of non-consuming rule: Str';
        self._ipg_start = Math.min(self._ipg_start, left + nt_Str._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_Str._ipg_end);
        nt_Str._ipg_end += left;
        nt_Str._ipg_start += left;
        self.values.push(nt_Str.string);
        left = nt_Str._ipg_end;
        right = EOI;
      }
    }

    // { section = values }
    self.section = self.values;

    return self;
  }

  
  return null;
}

function Str(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U8;
    let nt_NUL_BYTE;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat U8[U8.END, EOI].value starting on [0, EOI] until NUL_BYTE
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_NUL_BYTE = NUL_BYTE(input, begin + left, begin + right);
      if (nt_NUL_BYTE !== null) {
        if (nt_NUL_BYTE._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_NUL_BYTE._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_NUL_BYTE._ipg_end);
        }
        nt_NUL_BYTE._ipg_end += left;
        nt_NUL_BYTE._ipg_start += left;
        right = nt_NUL_BYTE._ipg_end;
        break;
      }
      nt_U8 = U8(input, begin + left, begin + right);
      if (nt_U8 === null) break _ipg_alt;
      if (nt_U8._ipg_end === 0) throw 'repeat of non-consuming rule: U8';
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8._ipg_end);
      nt_U8._ipg_end += left;
      nt_U8._ipg_start += left;
      self.values.push(nt_U8.value);
      left = nt_U8._ipg_end;
      right = EOI;
    }

    // { string = decodeAscii(values) }
    self.string = decodeAscii(self.values);

    return self;
  }

  
  return null;
}

function DynSymSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSymSecEntry;
    let seq_DynSymSecEntry; let seq_DynSymSecEntry_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 24 do DynSymSecEntry[24 * i, 24 * (i + 1)]
    nt_DynSymSecEntry = { _ipg_end: right, _ipg_start: left };
    seq_DynSymSecEntry_start = 0;
    loopEnd = EOI / 24;
    seq_DynSymSecEntry = new Array(loopEnd - seq_DynSymSecEntry_start);
    for (self.i = seq_DynSymSecEntry_start; self.i < loopEnd; self.i++) {
      const left = 24 * self.i;
      const right = 24 * (self.i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = DynSymSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DynSymSecEntry._ipg_end = tmp._ipg_end;
      nt_DynSymSecEntry._ipg_start = tmp._ipg_start;
      seq_DynSymSecEntry[self.i - seq_DynSymSecEntry_start] = tmp;
    }
    delete self.i;
    left = nt_DynSymSecEntry._ipg_start;
    right = nt_DynSymSecEntry._ipg_end;

    // { section = DynSymSecEntry.these }
    self.section = seq_DynSymSecEntry.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function DynSymSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32;
    let nt_U16;
    let nt_U64;
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

    // { sh_name = U32.value }
    self.sh_name = nt_U32.value;

    // { st_info = .[U32.END] }
    left = nt_U32._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.st_info = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { st_other = .[U32.END + 1] }
    left = nt_U32._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.st_other = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // U16[U32.END + 2, EOI]
    left = nt_U32._ipg_end + 2;
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

    // { st_shndx = U16.value }
    self.st_shndx = nt_U16.value;

    // U64[U16.END, EOI]
    left = nt_U16._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { st_value = U64.value }
    self.st_value = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { st_size = U64.value }
    self.st_size = nt_U64.value;

    return self;
  }

  
  return null;
}

function NoteSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NoteSecEntry;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat NoteSecEntry[NoteSecEntry.END, EOI].entry starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_NoteSecEntry = NoteSecEntry(input, begin + left, begin + right);
    if (nt_NoteSecEntry !== null) {
      if (nt_NoteSecEntry._ipg_end === 0) throw 'repeat of non-consuming rule: NoteSecEntry';
      self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSecEntry._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSecEntry._ipg_end);
      nt_NoteSecEntry._ipg_end += left;
      nt_NoteSecEntry._ipg_start += left;
      left = nt_NoteSecEntry._ipg_end;
      right = EOI;
      self.values.push(nt_NoteSecEntry.entry);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_NoteSecEntry = NoteSecEntry(input, begin + left, begin + right);
        if (nt_NoteSecEntry === null) break;
        if (nt_NoteSecEntry._ipg_end === 0) throw 'repeat of non-consuming rule: NoteSecEntry';
        self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSecEntry._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSecEntry._ipg_end);
        nt_NoteSecEntry._ipg_end += left;
        nt_NoteSecEntry._ipg_start += left;
        self.values.push(nt_NoteSecEntry.entry);
        left = nt_NoteSecEntry._ipg_end;
        right = EOI;
      }
    }

    // { section = values }
    self.section = self.values;

    return self;
  }

  
  return null;
}

function NoteSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32;
    let nt_Name;
    let nt_Descriptor;
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

    // { len_name = U32.value }
    self.len_name = nt_U32.value;

    // U32[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { len_descriptor = U32.value }
    self.len_descriptor = nt_U32.value;

    // U32[U32.END, EOI]
    left = nt_U32._ipg_end;
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

    // { type = U32.value }
    self.type = nt_U32.value;

    // Name[U32.END, U32.END + len_name]
    left = nt_U32._ipg_end;
    right = nt_U32._ipg_end + self.len_name;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Name = Name(input, begin + left, begin + right);
    if (nt_Name === null) break _ipg_alt;
    if (nt_Name._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Name._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Name._ipg_end);
    }
    nt_Name._ipg_end += left;
    nt_Name._ipg_start += left;
    left = nt_Name._ipg_start;
    right = nt_Name._ipg_end;

    // Descriptor[Name.END + (-len_name & 3), Name.END + (-len_name & 3) + len_descriptor]
    left = nt_Name._ipg_end + (-self.len_name & 3);
    right = nt_Name._ipg_end + (-self.len_name & 3) + self.len_descriptor;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Descriptor = Descriptor(input, begin + left, begin + right);
    if (nt_Descriptor === null) break _ipg_alt;
    if (nt_Descriptor._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Descriptor._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Descriptor._ipg_end);
    }
    nt_Descriptor._ipg_end += left;
    nt_Descriptor._ipg_start += left;
    left = nt_Descriptor._ipg_start;
    right = nt_Descriptor._ipg_end;

    // { entry = makeEntry(Name.value, Descriptor.value, type) }
    self.entry = makeEntry(nt_Name.value, nt_Descriptor.value, self.type);

    return self;
  }

  
  return null;
}

function Name(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // { value = *[0, EOI - 1] }
    left = 0;
    right = EOI - 1;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    self.value = input.slice(begin + left, begin + right);
    if (left !== right) {
      self._ipg_start = Math.min(self._ipg_start, left);
      self._ipg_end = Math.max(self._ipg_end, right);
    }

    // "\x00"[EOI - 1, EOI]
    left = EOI - 1;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x00")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);
    right = left + 1;

    return self;
  }

  
  return null;
}

function Descriptor(input, begin = 0, end = input.length) {
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

function RelAddEndSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelAddEndSecEntry;
    let seq_RelAddEndSecEntry; let seq_RelAddEndSecEntry_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 24 do RelAddEndSecEntry[24 * i, 24 * (i + 1)]
    nt_RelAddEndSecEntry = { _ipg_end: right, _ipg_start: left };
    seq_RelAddEndSecEntry_start = 0;
    loopEnd = EOI / 24;
    seq_RelAddEndSecEntry = new Array(loopEnd - seq_RelAddEndSecEntry_start);
    for (self.i = seq_RelAddEndSecEntry_start; self.i < loopEnd; self.i++) {
      const left = 24 * self.i;
      const right = 24 * (self.i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = RelAddEndSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RelAddEndSecEntry._ipg_end = tmp._ipg_end;
      nt_RelAddEndSecEntry._ipg_start = tmp._ipg_start;
      seq_RelAddEndSecEntry[self.i - seq_RelAddEndSecEntry_start] = tmp;
    }
    delete self.i;
    left = nt_RelAddEndSecEntry._ipg_start;
    right = nt_RelAddEndSecEntry._ipg_end;

    // { section = RelAddEndSecEntry.these }
    self.section = seq_RelAddEndSecEntry.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function RelAddEndSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { offset = U64.value }
    self.offset = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { info = U64.value }
    self.info = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { addend = U64.value }
    self.addend = nt_U64.value;

    return self;
  }

  
  return null;
}

function RelSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelSecEntry;
    let seq_RelSecEntry; let seq_RelSecEntry_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 16 do RelSecEntry[16 * i, 16 * (i + 1)]
    nt_RelSecEntry = { _ipg_end: right, _ipg_start: left };
    seq_RelSecEntry_start = 0;
    loopEnd = EOI / 16;
    seq_RelSecEntry = new Array(loopEnd - seq_RelSecEntry_start);
    for (self.i = seq_RelSecEntry_start; self.i < loopEnd; self.i++) {
      const left = 16 * self.i;
      const right = 16 * (self.i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = RelSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RelSecEntry._ipg_end = tmp._ipg_end;
      nt_RelSecEntry._ipg_start = tmp._ipg_start;
      seq_RelSecEntry[self.i - seq_RelSecEntry_start] = tmp;
    }
    delete self.i;
    left = nt_RelSecEntry._ipg_start;
    right = nt_RelSecEntry._ipg_end;

    // { section = RelSecEntry.these }
    self.section = seq_RelSecEntry.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function RelSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { offset = U64.value }
    self.offset = nt_U64.value;

    // U64[U64.END, EOI]
    left = nt_U64._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64 = U64(input, begin + left, begin + right);
    if (nt_U64 === null) break _ipg_alt;
    if (nt_U64._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64._ipg_end);
    }
    nt_U64._ipg_end += left;
    nt_U64._ipg_start += left;
    left = nt_U64._ipg_start;
    right = nt_U64._ipg_end;

    // { info = U64.value }
    self.info = nt_U64.value;

    return self;
  }

  
  return null;
}

console.log(JSON.stringify(ELF(fs.readFileSync("./elf_samples/1.elf"))));
