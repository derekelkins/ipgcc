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
    let nt_H_0;
    let nt_SH_0;
    let nt_Sec_0;
    let seq_SH_0; let seq_SH_0_start = 0;
    let seq_Sec_0; let seq_Sec_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // H@0[0, 128]
    left = 0;
    right = 128;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_H_0 = H(input, begin + left, begin + right);
    if (nt_H_0 === null) break _ipg_alt;
    if (nt_H_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_H_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_H_0._ipg_end);
    }
    nt_H_0._ipg_end += left;
    nt_H_0._ipg_start += left;
    left = nt_H_0._ipg_start;
    right = nt_H_0._ipg_end;

    // for i = 0 to H@0.e_shnum do SH@0[H@0.e_shoff + i * H@0.e_shentsize, H@0.e_shoff + (i + 1) * H@0.e_shentsize]
    nt_SH_0 = { _ipg_end: right, _ipg_start: left };
    seq_SH_0_start = 0;
    loopEnd = nt_H_0.e_shnum;
    seq_SH_0 = new Array(Math.max(0, loopEnd - seq_SH_0_start));
    for (let i_i = seq_SH_0_start; i_i < loopEnd; i_i++) {
      const left = nt_H_0.e_shoff + i_i * nt_H_0.e_shentsize;
      const right = nt_H_0.e_shoff + (i_i + 1) * nt_H_0.e_shentsize;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = SH(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_SH_0._ipg_end = tmp._ipg_end;
      nt_SH_0._ipg_start = tmp._ipg_start;
      seq_SH_0[i_i - seq_SH_0_start] = tmp;
    }
    left = nt_SH_0._ipg_start;
    right = nt_SH_0._ipg_end;

    // for i = 1 to H@0.e_shnum do Sec@0(SH@0(i).sh_type)[SH@0(i).sh_offset, SH@0(i).sh_offset + SH@0(i).sh_size]
    nt_Sec_0 = { _ipg_end: right, _ipg_start: left };
    seq_Sec_0_start = 1;
    loopEnd = nt_H_0.e_shnum;
    seq_Sec_0 = new Array(Math.max(0, loopEnd - seq_Sec_0_start));
    for (let i_i = seq_Sec_0_start; i_i < loopEnd; i_i++) {
      const left = seq_SH_0[i_i - seq_SH_0_start].sh_offset;
      const right = seq_SH_0[i_i - seq_SH_0_start].sh_offset + seq_SH_0[i_i - seq_SH_0_start].sh_size;
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = Sec(input, begin + left, begin + right, seq_SH_0[i_i - seq_SH_0_start].sh_type);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_Sec_0._ipg_end = tmp._ipg_end;
      nt_Sec_0._ipg_start = tmp._ipg_start;
      seq_Sec_0[i_i - seq_Sec_0_start] = tmp;
    }
    left = nt_Sec_0._ipg_start;
    right = nt_Sec_0._ipg_end;

    // { header = H@0.this }
    self.header = (({_ipg_start,_ipg_end,...o}) => o)(nt_H_0);

    // { section_headers = SH@0.these }
    self.section_headers = seq_SH_0.map(({_ipg_start,_ipg_end,...o}) => o);

    // { sections = projectSections(Sec@0.these) }
    self.sections = projectSections(seq_Sec_0.map(({_ipg_start,_ipg_end,...o}) => o));

    return self;
  }

  
  return null;
}

function Sec(input, begin = 0, end = input.length, a_sh_type) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 6 ]
    if (!(a_sh_type == 6)) break _ipg_alt;

    // DynSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DynSec_0 = DynSec(input, begin + left, begin + right);
    if (nt_DynSec_0 === null) break _ipg_alt;
    if (nt_DynSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DynSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DynSec_0._ipg_end);
    }
    nt_DynSec_0._ipg_end += left;
    nt_DynSec_0._ipg_start += left;
    left = nt_DynSec_0._ipg_start;
    right = nt_DynSec_0._ipg_end;

    // { section = DynSec@0.section }
    self.section = nt_DynSec_0.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_StrSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 3 ]
    if (!(a_sh_type == 3)) break _ipg_alt;

    // StrSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_StrSec_0 = StrSec(input, begin + left, begin + right);
    if (nt_StrSec_0 === null) break _ipg_alt;
    if (nt_StrSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_StrSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_StrSec_0._ipg_end);
    }
    nt_StrSec_0._ipg_end += left;
    nt_StrSec_0._ipg_start += left;
    left = nt_StrSec_0._ipg_start;
    right = nt_StrSec_0._ipg_end;

    // { section = StrSec@0.section }
    self.section = nt_StrSec_0.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSymSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 11 || sh_type == 2 ]
    if (!(a_sh_type == 11 || a_sh_type == 2)) break _ipg_alt;

    // DynSymSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_DynSymSec_0 = DynSymSec(input, begin + left, begin + right);
    if (nt_DynSymSec_0 === null) break _ipg_alt;
    if (nt_DynSymSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_DynSymSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_DynSymSec_0._ipg_end);
    }
    nt_DynSymSec_0._ipg_end += left;
    nt_DynSymSec_0._ipg_start += left;
    left = nt_DynSymSec_0._ipg_start;
    right = nt_DynSymSec_0._ipg_end;

    // { section = DynSymSec@0.section }
    self.section = nt_DynSymSec_0.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NoteSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 7 ]
    if (!(a_sh_type == 7)) break _ipg_alt;

    // NoteSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_NoteSec_0 = NoteSec(input, begin + left, begin + right);
    if (nt_NoteSec_0 === null) break _ipg_alt;
    if (nt_NoteSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSec_0._ipg_end);
    }
    nt_NoteSec_0._ipg_end += left;
    nt_NoteSec_0._ipg_start += left;
    left = nt_NoteSec_0._ipg_start;
    right = nt_NoteSec_0._ipg_end;

    // { section = NoteSec@0.section }
    self.section = nt_NoteSec_0.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 9 ]
    if (!(a_sh_type == 9)) break _ipg_alt;

    // RelSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RelSec_0 = RelSec(input, begin + left, begin + right);
    if (nt_RelSec_0 === null) break _ipg_alt;
    if (nt_RelSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RelSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RelSec_0._ipg_end);
    }
    nt_RelSec_0._ipg_end += left;
    nt_RelSec_0._ipg_start += left;
    left = nt_RelSec_0._ipg_start;
    right = nt_RelSec_0._ipg_end;

    // { section = RelSec@0.section }
    self.section = nt_RelSec_0.section;

    return self;
  }
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelAddEndSec_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // ?[ sh_type == 4 ]
    if (!(a_sh_type == 4)) break _ipg_alt;

    // RelAddEndSec@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_RelAddEndSec_0 = RelAddEndSec(input, begin + left, begin + right);
    if (nt_RelAddEndSec_0 === null) break _ipg_alt;
    if (nt_RelAddEndSec_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_RelAddEndSec_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_RelAddEndSec_0._ipg_end);
    }
    nt_RelAddEndSec_0._ipg_end += left;
    nt_RelAddEndSec_0._ipg_start += left;
    left = nt_RelAddEndSec_0._ipg_start;
    right = nt_RelAddEndSec_0._ipg_end;

    // { section = RelAddEndSec@0.section }
    self.section = nt_RelAddEndSec_0.section;

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
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

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
    let nt_U16_0;
    let nt_U16_1;
    let nt_U32_0;
    let nt_U64_0;
    let nt_U64_1;
    let nt_U64_2;
    let nt_U32_1;
    let nt_U16_2;
    let nt_U16_3;
    let nt_U16_4;
    let nt_U16_5;
    let nt_U16_6;
    let nt_U16_7;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // "\x7fELF"[0, 4]
    left = 0;
    right = 4;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    if (!_ipg_startsWith(input, begin + left, begin + right, "\x7fELF")) break _ipg_alt;
    self._ipg_start = Math.min(self._ipg_start, left);
    right = left + 4;
    self._ipg_end = Math.max(self._ipg_end, right);

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

    // U16@0[16, 18]
    left = 16;
    right = 18;
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

    // { e_type = U16@0.value }
    self.e_type = nt_U16_0.value;

    // U16@1[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
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

    // { e_machine = U16@1.value }
    self.e_machine = nt_U16_1.value;

    // U32@0[U16@1.END, EOI]
    left = nt_U16_1._ipg_end;
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

    // { e_version = U32@0.value }
    self.e_version = nt_U32_0.value;

    // U64@0[U32@0.END, EOI]
    left = nt_U32_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { e_entry = U64@0.value }
    self.e_entry = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { e_phoff = U64@1.value }
    self.e_phoff = nt_U64_1.value;

    // U64@2[U64@1.END, EOI]
    left = nt_U64_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_2 = U64(input, begin + left, begin + right);
    if (nt_U64_2 === null) break _ipg_alt;
    if (nt_U64_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_2._ipg_end);
    }
    nt_U64_2._ipg_end += left;
    nt_U64_2._ipg_start += left;
    left = nt_U64_2._ipg_start;
    right = nt_U64_2._ipg_end;

    // { e_shoff = U64@2.value }
    self.e_shoff = nt_U64_2.value;

    // U32@1[U64@2.END, EOI]
    left = nt_U64_2._ipg_end;
    right = EOI;
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

    // { e_flags = U32@1.value }
    self.e_flags = nt_U32_1.value;

    // U16@2[U32@1.END, EOI]
    left = nt_U32_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_2 = U16(input, begin + left, begin + right);
    if (nt_U16_2 === null) break _ipg_alt;
    if (nt_U16_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_2._ipg_end);
    }
    nt_U16_2._ipg_end += left;
    nt_U16_2._ipg_start += left;
    left = nt_U16_2._ipg_start;
    right = nt_U16_2._ipg_end;

    // { e_ehsize = U16@2.value }
    self.e_ehsize = nt_U16_2.value;

    // U16@3[U16@2.END, EOI]
    left = nt_U16_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_3 = U16(input, begin + left, begin + right);
    if (nt_U16_3 === null) break _ipg_alt;
    if (nt_U16_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_3._ipg_end);
    }
    nt_U16_3._ipg_end += left;
    nt_U16_3._ipg_start += left;
    left = nt_U16_3._ipg_start;
    right = nt_U16_3._ipg_end;

    // { e_phentsize = U16@3.value }
    self.e_phentsize = nt_U16_3.value;

    // U16@4[U16@3.END, EOI]
    left = nt_U16_3._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_4 = U16(input, begin + left, begin + right);
    if (nt_U16_4 === null) break _ipg_alt;
    if (nt_U16_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_4._ipg_end);
    }
    nt_U16_4._ipg_end += left;
    nt_U16_4._ipg_start += left;
    left = nt_U16_4._ipg_start;
    right = nt_U16_4._ipg_end;

    // { e_phnum = U16@4.value }
    self.e_phnum = nt_U16_4.value;

    // U16@5[U16@4.END, EOI]
    left = nt_U16_4._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_5 = U16(input, begin + left, begin + right);
    if (nt_U16_5 === null) break _ipg_alt;
    if (nt_U16_5._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_5._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_5._ipg_end);
    }
    nt_U16_5._ipg_end += left;
    nt_U16_5._ipg_start += left;
    left = nt_U16_5._ipg_start;
    right = nt_U16_5._ipg_end;

    // { e_shentsize = U16@5.value }
    self.e_shentsize = nt_U16_5.value;

    // U16@6[U16@5.END, EOI]
    left = nt_U16_5._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_6 = U16(input, begin + left, begin + right);
    if (nt_U16_6 === null) break _ipg_alt;
    if (nt_U16_6._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_6._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_6._ipg_end);
    }
    nt_U16_6._ipg_end += left;
    nt_U16_6._ipg_start += left;
    left = nt_U16_6._ipg_start;
    right = nt_U16_6._ipg_end;

    // { e_shnum = U16@6.value }
    self.e_shnum = nt_U16_6.value;

    // U16@7[U16@6.END, EOI]
    left = nt_U16_6._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U16_7 = U16(input, begin + left, begin + right);
    if (nt_U16_7 === null) break _ipg_alt;
    if (nt_U16_7._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U16_7._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U16_7._ipg_end);
    }
    nt_U16_7._ipg_end += left;
    nt_U16_7._ipg_start += left;
    left = nt_U16_7._ipg_start;
    right = nt_U16_7._ipg_end;

    // { e_shstrndx = U16@7.value }
    self.e_shstrndx = nt_U16_7.value;

    return self;
  }

  
  return null;
}

function SH(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32_0;
    let nt_U32_1;
    let nt_U64_0;
    let nt_U64_1;
    let nt_U64_2;
    let nt_U64_3;
    let nt_U32_2;
    let nt_U32_3;
    let nt_U32_4;
    let nt_U64_4;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U32@0[0, EOI]
    left = 0;
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

    // { sh_name = U32@0.value }
    self.sh_name = nt_U32_0.value;

    // U32@1[U32@0.END, EOI]
    left = nt_U32_0._ipg_end;
    right = EOI;
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

    // { sh_type = U32@1.value }
    self.sh_type = nt_U32_1.value;

    // U64@0[U32@1.END, EOI]
    left = nt_U32_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { sh_flags = U64@0.value }
    self.sh_flags = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { sh_addr = U64@1.value }
    self.sh_addr = nt_U64_1.value;

    // U64@2[U64@1.END, EOI]
    left = nt_U64_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_2 = U64(input, begin + left, begin + right);
    if (nt_U64_2 === null) break _ipg_alt;
    if (nt_U64_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_2._ipg_end);
    }
    nt_U64_2._ipg_end += left;
    nt_U64_2._ipg_start += left;
    left = nt_U64_2._ipg_start;
    right = nt_U64_2._ipg_end;

    // { sh_offset = U64@2.value }
    self.sh_offset = nt_U64_2.value;

    // U64@3[U64@2.END, EOI]
    left = nt_U64_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_3 = U64(input, begin + left, begin + right);
    if (nt_U64_3 === null) break _ipg_alt;
    if (nt_U64_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_3._ipg_end);
    }
    nt_U64_3._ipg_end += left;
    nt_U64_3._ipg_start += left;
    left = nt_U64_3._ipg_start;
    right = nt_U64_3._ipg_end;

    // { sh_size = U64@3.value }
    self.sh_size = nt_U64_3.value;

    // U32@2[U64@3.END, EOI]
    left = nt_U64_3._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_2 = U32(input, begin + left, begin + right);
    if (nt_U32_2 === null) break _ipg_alt;
    if (nt_U32_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_2._ipg_end);
    }
    nt_U32_2._ipg_end += left;
    nt_U32_2._ipg_start += left;
    left = nt_U32_2._ipg_start;
    right = nt_U32_2._ipg_end;

    // { sh_link = U32@2.value }
    self.sh_link = nt_U32_2.value;

    // U32@3[U32@2.END, EOI]
    left = nt_U32_2._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_3 = U32(input, begin + left, begin + right);
    if (nt_U32_3 === null) break _ipg_alt;
    if (nt_U32_3._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_3._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_3._ipg_end);
    }
    nt_U32_3._ipg_end += left;
    nt_U32_3._ipg_start += left;
    left = nt_U32_3._ipg_start;
    right = nt_U32_3._ipg_end;

    // { sh_info = U32@3.value }
    self.sh_info = nt_U32_3.value;

    // U32@4[U32@3.END, EOI]
    left = nt_U32_3._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_4 = U32(input, begin + left, begin + right);
    if (nt_U32_4 === null) break _ipg_alt;
    if (nt_U32_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_4._ipg_end);
    }
    nt_U32_4._ipg_end += left;
    nt_U32_4._ipg_start += left;
    left = nt_U32_4._ipg_start;
    right = nt_U32_4._ipg_end;

    // { sh_addralign = U32@4.value }
    self.sh_addralign = nt_U32_4.value;

    // U64@4[U32@4.END, EOI]
    left = nt_U32_4._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_4 = U64(input, begin + left, begin + right);
    if (nt_U64_4 === null) break _ipg_alt;
    if (nt_U64_4._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_4._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_4._ipg_end);
    }
    nt_U64_4._ipg_end += left;
    nt_U64_4._ipg_start += left;
    left = nt_U64_4._ipg_start;
    right = nt_U64_4._ipg_end;

    // { sh_entsize = U64@4.value }
    self.sh_entsize = nt_U64_4.value;

    return self;
  }

  
  return null;
}

function DynSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_DynSecEntry_0;
    let seq_DynSecEntry_0; let seq_DynSecEntry_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 16 do DynSecEntry@0[16 * i, 16 * (i + 1)]
    nt_DynSecEntry_0 = { _ipg_end: right, _ipg_start: left };
    seq_DynSecEntry_0_start = 0;
    loopEnd = EOI / 16;
    seq_DynSecEntry_0 = new Array(Math.max(0, loopEnd - seq_DynSecEntry_0_start));
    for (let i_i = seq_DynSecEntry_0_start; i_i < loopEnd; i_i++) {
      const left = 16 * i_i;
      const right = 16 * (i_i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = DynSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DynSecEntry_0._ipg_end = tmp._ipg_end;
      nt_DynSecEntry_0._ipg_start = tmp._ipg_start;
      seq_DynSecEntry_0[i_i - seq_DynSecEntry_0_start] = tmp;
    }
    left = nt_DynSecEntry_0._ipg_start;
    right = nt_DynSecEntry_0._ipg_end;

    // { section = DynSecEntry@0.these }
    self.section = seq_DynSecEntry_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function DynSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64_0;
    let nt_U64_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { tag = U64@0.value }
    self.tag = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { value_or_ptr = U64@1.value }
    self.value_or_ptr = nt_U64_1.value;

    return self;
  }

  
  return null;
}

function StrSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_Str_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat Str@0[Str@0.END, EOI].string starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_Str_0 = Str(input, begin + left, begin + right);
    if (nt_Str_0 !== null) {
      if (nt_Str_0._ipg_end === 0) throw 'repeat of non-consuming rule: Str';
      self._ipg_start = Math.min(self._ipg_start, left + nt_Str_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Str_0._ipg_end);
      nt_Str_0._ipg_end += left;
      nt_Str_0._ipg_start += left;
      left = nt_Str_0._ipg_end;
      right = EOI;
      self.values.push(nt_Str_0.string);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_Str_0 = Str(input, begin + left, begin + right);
        if (nt_Str_0 === null) break;
        if (nt_Str_0._ipg_end === 0) throw 'repeat of non-consuming rule: Str';
        self._ipg_start = Math.min(self._ipg_start, left + nt_Str_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_Str_0._ipg_end);
        nt_Str_0._ipg_end += left;
        nt_Str_0._ipg_start += left;
        self.values.push(nt_Str_0.string);
        left = nt_Str_0._ipg_end;
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
    let nt_U8_0;
    let nt_NUL_BYTE_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat U8@0[U8@0.END, EOI].value starting on [0, EOI] until NUL_BYTE@0
    left = 0;
    right = EOI;
    self.values = [];
    while (true) {
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      nt_NUL_BYTE_0 = NUL_BYTE(input, begin + left, begin + right);
      if (nt_NUL_BYTE_0 !== null) {
        if (nt_NUL_BYTE_0._ipg_end !== 0) {
          self._ipg_start = Math.min(self._ipg_start, left + nt_NUL_BYTE_0._ipg_start);
          self._ipg_end = Math.max(self._ipg_end, left + nt_NUL_BYTE_0._ipg_end);
        }
        nt_NUL_BYTE_0._ipg_end += left;
        nt_NUL_BYTE_0._ipg_start += left;
        right = nt_NUL_BYTE_0._ipg_end;
        break;
      }
      nt_U8_0 = U8(input, begin + left, begin + right);
      if (nt_U8_0 === null) break _ipg_alt;
      if (nt_U8_0._ipg_end === 0) throw 'repeat of non-consuming rule: U8';
      self._ipg_start = Math.min(self._ipg_start, left + nt_U8_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U8_0._ipg_end);
      nt_U8_0._ipg_end += left;
      nt_U8_0._ipg_start += left;
      self.values.push(nt_U8_0.value);
      left = nt_U8_0._ipg_end;
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
    let nt_DynSymSecEntry_0;
    let seq_DynSymSecEntry_0; let seq_DynSymSecEntry_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 24 do DynSymSecEntry@0[24 * i, 24 * (i + 1)]
    nt_DynSymSecEntry_0 = { _ipg_end: right, _ipg_start: left };
    seq_DynSymSecEntry_0_start = 0;
    loopEnd = EOI / 24;
    seq_DynSymSecEntry_0 = new Array(Math.max(0, loopEnd - seq_DynSymSecEntry_0_start));
    for (let i_i = seq_DynSymSecEntry_0_start; i_i < loopEnd; i_i++) {
      const left = 24 * i_i;
      const right = 24 * (i_i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = DynSymSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_DynSymSecEntry_0._ipg_end = tmp._ipg_end;
      nt_DynSymSecEntry_0._ipg_start = tmp._ipg_start;
      seq_DynSymSecEntry_0[i_i - seq_DynSymSecEntry_0_start] = tmp;
    }
    left = nt_DynSymSecEntry_0._ipg_start;
    right = nt_DynSymSecEntry_0._ipg_end;

    // { section = DynSymSecEntry@0.these }
    self.section = seq_DynSymSecEntry_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function DynSymSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U32_0;
    let nt_U16_0;
    let nt_U64_0;
    let nt_U64_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U32@0[0, EOI]
    left = 0;
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

    // { sh_name = U32@0.value }
    self.sh_name = nt_U32_0.value;

    // { st_info = .[U32@0.END] }
    left = nt_U32_0._ipg_end;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.st_info = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // { st_other = .[U32@0.END + 1] }
    left = nt_U32_0._ipg_end + 1;
    right = left + 1;
    if (left < 0 || right > EOI) break _ipg_alt;
    self.st_other = input[begin + left];
    self._ipg_start = Math.min(self._ipg_start, left);
    self._ipg_end = Math.max(self._ipg_end, right);

    // U16@0[U32@0.END + 2, EOI]
    left = nt_U32_0._ipg_end + 2;
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

    // { st_shndx = U16@0.value }
    self.st_shndx = nt_U16_0.value;

    // U64@0[U16@0.END, EOI]
    left = nt_U16_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { st_value = U64@0.value }
    self.st_value = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { st_size = U64@1.value }
    self.st_size = nt_U64_1.value;

    return self;
  }

  
  return null;
}

function NoteSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_NoteSecEntry_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // repeat NoteSecEntry@0[NoteSecEntry@0.END, EOI].entry starting on [0, EOI]
    self.values = [];
    left = 0;
    right = EOI;
    nt_NoteSecEntry_0 = NoteSecEntry(input, begin + left, begin + right);
    if (nt_NoteSecEntry_0 !== null) {
      if (nt_NoteSecEntry_0._ipg_end === 0) throw 'repeat of non-consuming rule: NoteSecEntry';
      self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSecEntry_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSecEntry_0._ipg_end);
      nt_NoteSecEntry_0._ipg_end += left;
      nt_NoteSecEntry_0._ipg_start += left;
      left = nt_NoteSecEntry_0._ipg_end;
      right = EOI;
      self.values.push(nt_NoteSecEntry_0.entry);

      while (left >= 0 && left <= right && right <= EOI) {
        nt_NoteSecEntry_0 = NoteSecEntry(input, begin + left, begin + right);
        if (nt_NoteSecEntry_0 === null) break;
        if (nt_NoteSecEntry_0._ipg_end === 0) throw 'repeat of non-consuming rule: NoteSecEntry';
        self._ipg_start = Math.min(self._ipg_start, left + nt_NoteSecEntry_0._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + nt_NoteSecEntry_0._ipg_end);
        nt_NoteSecEntry_0._ipg_end += left;
        nt_NoteSecEntry_0._ipg_start += left;
        self.values.push(nt_NoteSecEntry_0.entry);
        left = nt_NoteSecEntry_0._ipg_end;
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
    let nt_U32_0;
    let nt_U32_1;
    let nt_U32_2;
    let nt_Name_0;
    let nt_Descriptor_0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U32@0[0, EOI]
    left = 0;
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

    // { len_name = U32@0.value }
    self.len_name = nt_U32_0.value;

    // U32@1[U32@0.END, EOI]
    left = nt_U32_0._ipg_end;
    right = EOI;
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

    // { len_descriptor = U32@1.value }
    self.len_descriptor = nt_U32_1.value;

    // U32@2[U32@1.END, EOI]
    left = nt_U32_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U32_2 = U32(input, begin + left, begin + right);
    if (nt_U32_2 === null) break _ipg_alt;
    if (nt_U32_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U32_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U32_2._ipg_end);
    }
    nt_U32_2._ipg_end += left;
    nt_U32_2._ipg_start += left;
    left = nt_U32_2._ipg_start;
    right = nt_U32_2._ipg_end;

    // { type = U32@2.value }
    self.type = nt_U32_2.value;

    // Name@0[U32@2.END, U32@2.END + len_name]
    left = nt_U32_2._ipg_end;
    right = nt_U32_2._ipg_end + self.len_name;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Name_0 = Name(input, begin + left, begin + right);
    if (nt_Name_0 === null) break _ipg_alt;
    if (nt_Name_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Name_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Name_0._ipg_end);
    }
    nt_Name_0._ipg_end += left;
    nt_Name_0._ipg_start += left;
    left = nt_Name_0._ipg_start;
    right = nt_Name_0._ipg_end;

    // Descriptor@0[Name@0.END + (-len_name & 3), Name@0.END + (-len_name & 3) + len_descriptor]
    left = nt_Name_0._ipg_end + (-self.len_name & 3);
    right = nt_Name_0._ipg_end + (-self.len_name & 3) + self.len_descriptor;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_Descriptor_0 = Descriptor(input, begin + left, begin + right);
    if (nt_Descriptor_0 === null) break _ipg_alt;
    if (nt_Descriptor_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_Descriptor_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_Descriptor_0._ipg_end);
    }
    nt_Descriptor_0._ipg_end += left;
    nt_Descriptor_0._ipg_start += left;
    left = nt_Descriptor_0._ipg_start;
    right = nt_Descriptor_0._ipg_end;

    // { entry = makeEntry(Name@0.value, Descriptor@0.value, type) }
    self.entry = makeEntry(nt_Name_0.value, nt_Descriptor_0.value, self.type);

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
    right = left + 1;
    self._ipg_end = Math.max(self._ipg_end, right);

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
    let nt_RelAddEndSecEntry_0;
    let seq_RelAddEndSecEntry_0; let seq_RelAddEndSecEntry_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 24 do RelAddEndSecEntry@0[24 * i, 24 * (i + 1)]
    nt_RelAddEndSecEntry_0 = { _ipg_end: right, _ipg_start: left };
    seq_RelAddEndSecEntry_0_start = 0;
    loopEnd = EOI / 24;
    seq_RelAddEndSecEntry_0 = new Array(Math.max(0, loopEnd - seq_RelAddEndSecEntry_0_start));
    for (let i_i = seq_RelAddEndSecEntry_0_start; i_i < loopEnd; i_i++) {
      const left = 24 * i_i;
      const right = 24 * (i_i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = RelAddEndSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RelAddEndSecEntry_0._ipg_end = tmp._ipg_end;
      nt_RelAddEndSecEntry_0._ipg_start = tmp._ipg_start;
      seq_RelAddEndSecEntry_0[i_i - seq_RelAddEndSecEntry_0_start] = tmp;
    }
    left = nt_RelAddEndSecEntry_0._ipg_start;
    right = nt_RelAddEndSecEntry_0._ipg_end;

    // { section = RelAddEndSecEntry@0.these }
    self.section = seq_RelAddEndSecEntry_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function RelAddEndSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64_0;
    let nt_U64_1;
    let nt_U64_2;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { offset = U64@0.value }
    self.offset = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { info = U64@1.value }
    self.info = nt_U64_1.value;

    // U64@2[U64@1.END, EOI]
    left = nt_U64_1._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_2 = U64(input, begin + left, begin + right);
    if (nt_U64_2 === null) break _ipg_alt;
    if (nt_U64_2._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_2._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_2._ipg_end);
    }
    nt_U64_2._ipg_end += left;
    nt_U64_2._ipg_start += left;
    left = nt_U64_2._ipg_start;
    right = nt_U64_2._ipg_end;

    // { addend = U64@2.value }
    self.addend = nt_U64_2.value;

    return self;
  }

  
  return null;
}

function RelSec(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_RelSecEntry_0;
    let seq_RelSecEntry_0; let seq_RelSecEntry_0_start = 0;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // for i = 0 to EOI / 16 do RelSecEntry@0[16 * i, 16 * (i + 1)]
    nt_RelSecEntry_0 = { _ipg_end: right, _ipg_start: left };
    seq_RelSecEntry_0_start = 0;
    loopEnd = EOI / 16;
    seq_RelSecEntry_0 = new Array(Math.max(0, loopEnd - seq_RelSecEntry_0_start));
    for (let i_i = seq_RelSecEntry_0_start; i_i < loopEnd; i_i++) {
      const left = 16 * i_i;
      const right = 16 * (i_i + 1);
      if (left < 0 || right < left || right > EOI) break _ipg_alt;
      const tmp = RelSecEntry(input, begin + left, begin + right);
      if (tmp === null) break _ipg_alt;
      if (tmp._ipg_end !== 0) {
        self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);
        self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);
      }
      tmp._ipg_end += left;
      tmp._ipg_start += left;
      nt_RelSecEntry_0._ipg_end = tmp._ipg_end;
      nt_RelSecEntry_0._ipg_start = tmp._ipg_start;
      seq_RelSecEntry_0[i_i - seq_RelSecEntry_0_start] = tmp;
    }
    left = nt_RelSecEntry_0._ipg_start;
    right = nt_RelSecEntry_0._ipg_end;

    // { section = RelSecEntry@0.these }
    self.section = seq_RelSecEntry_0.map(({_ipg_start,_ipg_end,...o}) => o);

    return self;
  }

  
  return null;
}

function RelSecEntry(input, begin = 0, end = input.length) {
  const EOI = end - begin; let self;
  
  _ipg_alt: {
    let left = EOI; let right = 0; let loopEnd = 0;
    let nt_U64_0;
    let nt_U64_1;
    self = { _ipg_start: EOI, _ipg_end: 0 };

    // U64@0[0, EOI]
    left = 0;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_0 = U64(input, begin + left, begin + right);
    if (nt_U64_0 === null) break _ipg_alt;
    if (nt_U64_0._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_0._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_0._ipg_end);
    }
    nt_U64_0._ipg_end += left;
    nt_U64_0._ipg_start += left;
    left = nt_U64_0._ipg_start;
    right = nt_U64_0._ipg_end;

    // { offset = U64@0.value }
    self.offset = nt_U64_0.value;

    // U64@1[U64@0.END, EOI]
    left = nt_U64_0._ipg_end;
    right = EOI;
    if (left < 0 || right < left || right > EOI) break _ipg_alt;
    nt_U64_1 = U64(input, begin + left, begin + right);
    if (nt_U64_1 === null) break _ipg_alt;
    if (nt_U64_1._ipg_end !== 0) {
      self._ipg_start = Math.min(self._ipg_start, left + nt_U64_1._ipg_start);
      self._ipg_end = Math.max(self._ipg_end, left + nt_U64_1._ipg_end);
    }
    nt_U64_1._ipg_end += left;
    nt_U64_1._ipg_start += left;
    left = nt_U64_1._ipg_start;
    right = nt_U64_1._ipg_end;

    // { info = U64@1.value }
    self.info = nt_U64_1.value;

    return self;
  }

  
  return null;
}

console.log(JSON.stringify(ELF(fs.readFileSync("./elf_samples/1.elf"))));
