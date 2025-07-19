#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unreachable_code)]
#![allow(unused_mut)]
#![allow(unused_assignments)]
#![allow(unused_variables)]
#![allow(non_camel_case_types)]

type AString = String;

fn decodeAscii(bytes: Vec<u8>) -> String {
    if bytes.len() == 1 {
        match bytes[0] {
            0 => return ".".to_string(),
            1 => return "..".to_string(),
            _ => {}
        }
    }
    String::from_utf8(bytes).unwrap().trim_end().to_string()
}
fn decodeUtf16(bytes: Vec<u8>) -> String {
    if bytes.len() == 1 {
        match bytes[0] {
            0 => return ".".to_string(),
            1 => return "..".to_string(),
            _ => {}
        }
    }
    let words: Vec<u16> = bytes
        .as_slice()
        .as_chunks::<2>().0
        .into_iter()
        .map(|&[hi, lo]| ((hi as u16) << 8) | (lo as u16))
        .collect();
    String::from_utf16(&words).unwrap().trim_end().to_string()
}
fn decodeAscii2(bytes: Vec<u8>) -> String { decodeAscii(bytes) }
fn length<a>(xs: &[a]) -> usize { xs.len() }
fn asHex(bs: Vec<u8>) -> String {
    bs.into_iter().map(|b| format!("{:02X?}", b)).collect::<String>()
}

#[derive(Clone, Debug)]
struct PrimaryVolumeDescriptor {
    systemIdentifier: String,
    volumeIdentifier: String,
    volumeSpaceSize:  i64,
    volumeSetSize:  i64,
    volumeSequenceNumber:  i64,
    logicalBlockSize:  i64,
    pathTableSize:  i64,
    locationOfTypeLPathTable:  i64,
    locationOfOptionalTypeLPathTable:  i64,
    locationOfTypeMPathTable:  i64,
    locationOfOptionalTypeMPathTable:  i64,
    rootDirectoryRecord: DirectoryRecord,
    volumeSetIdentifier: String,
    publisherIdentifier: String,
    dataPreparerIdentifier: String,
    applicationIdentifier: String,
    copyrightFileIdentifier: String,
    abstractFileIdentifier: String,
    bibliographicFileIdentifier: String,
    volumeCreationDateAndTime: DateAndTime,
    volumeModificationDateAndTime: DateAndTime,
    volumeExpirationDateAndTime: DateAndTime,
    volumeEffectiveDateAndTime: DateAndTime,
    applicationUse: String
}

#[derive(Clone, Debug)]
enum TaggedVolumeDescriptor {
    Primary(PrimaryVolumeDescriptor),
    Supplementary {
        volumeDescriptorVersion: u8,
        volumeFlags: u8,
        systemIdentifier: String,
        volumeIdentifier: String,
        volumeSpaceSize: i64,
        escapeSequences: String,
        volumeSetSize: i64,
        volumeSequenceNumber: i64,
        logicalBlockSize: i64,
        pathTableSize: i64,
        locationOfTypeLPathTable: i64,
        locationOfOptionalTypeLPathTable: i64,
        locationOfTypeMPathTable: i64,
        locationOfOptionalTypeMPathTable: i64,
        rootDirectoryRecord: DirectoryRecord,
        volumeSetIdentifier: String,
        publisherIdentifier: String,
        dataPreparerIdentifier: String,
        applicationIdentifier: String,
        copyrightFileIdentifier: String,
        abstractFileIdentifier: String,
        bibliographicFileIdentifier: String,
        volumeCreationDateAndTime: DateAndTime,
        volumeModificationDateAndTime: DateAndTime,
        volumeExpirationDateAndTime: DateAndTime,
        volumeEffectiveDateAndTime: DateAndTime,
        fileStructureVersion: u8,
        applicationUse: String
    },
    VolumePartition {
        systemIdentifier: String,
        volumePartitionIdentifier: String,
        volumePartitionLocation: i64,
        volumePartitionSize: i64,
        systemUse: String
    },
    Boot {
        bootSystemIdentifier: String,
        bootIdentifier: String,
        systemUse: String
    },
    Terminator(),
    Unknown(Vec<u8>),
}
use TaggedVolumeDescriptor::*;

#[derive(Clone, Debug)]
enum DirectoryTree {
    DirectoryBranch(i64, DirectoryRecord, Vec<DirectoryTree>),
    FileLeaf(i64, DirectoryRecord),
}
use DirectoryTree::*;

fn projectRoot(drs: Vec<DirectoriesRecursive>) -> Vec<DirectoryTree> {
    drs.into_iter().map(|dr| dr.root).collect()
}

fn getPrimaryDescriptor(descriptors: &[TaggedVolumeDescriptor]) -> PrimaryVolumeDescriptor {
    for descriptor in descriptors.iter() {
        match descriptor {
            Primary(pvd) => { return pvd.clone(); }
            _ => {}
        }
    }
    panic!("Couldn't find a primary descriptor")
}

fn logicalBlockSize(descriptor: &PrimaryVolumeDescriptor) -> i64 { descriptor.logicalBlockSize }
fn rootDirectoryRecord(descriptor: &PrimaryVolumeDescriptor) -> DirectoryRecord {
    descriptor.rootDirectoryRecord.clone()
}
fn locationOfTypeLPathTable(descriptor: &PrimaryVolumeDescriptor) -> i64 {
    descriptor.locationOfTypeLPathTable
}
fn pathTableSize(descriptor: &PrimaryVolumeDescriptor) -> i64 { descriptor.pathTableSize }
fn isDirectory(record: &DirectoryRecord) -> bool { record.isDirectory }
fn locationOfExtent(record: &DirectoryRecord) -> i64 { record.locationOfExtent }
fn dataLength(record: &DirectoryRecord) -> i64 { record.dataLength }
fn locationOfExtentPT(record: &PathTableRecord) -> i64 { record.locationOfExtent }
fn clone<a: Clone>(x: &a) -> a { x.clone() }

fn makePrimaryVolumeDescriptor(
    systemIdentifier: String,
    volumeIdentifier: String,
    volumeSpaceSize:  i64,
    volumeSetSize:  i64,
    volumeSequenceNumber:  i64,
    logicalBlockSize:  i64,
    pathTableSize:  i64,
    locationOfTypeLPathTable:  i64,
    locationOfOptionalTypeLPathTable:  i64,
    locationOfTypeMPathTable:  i64,
    locationOfOptionalTypeMPathTable:  i64,
    rootDirectoryRecord: DirectoryRecord,
    volumeSetIdentifier: String,
    publisherIdentifier: String,
    dataPreparerIdentifier: String,
    applicationIdentifier: String,
    copyrightFileIdentifier: String,
    abstractFileIdentifier: String,
    bibliographicFileIdentifier: String,
    volumeCreationDateAndTime: DateAndTime,
    volumeModificationDateAndTime: DateAndTime,
    volumeExpirationDateAndTime: DateAndTime,
    volumeEffectiveDateAndTime: DateAndTime,
    applicationUse: String
) -> TaggedVolumeDescriptor {
    Primary(PrimaryVolumeDescriptor {
        systemIdentifier,
        volumeIdentifier,
        volumeSpaceSize,
        volumeSetSize,
        volumeSequenceNumber,
        logicalBlockSize,
        pathTableSize,
        locationOfTypeLPathTable,
        locationOfOptionalTypeLPathTable,
        locationOfTypeMPathTable,
        locationOfOptionalTypeMPathTable,
        rootDirectoryRecord,
        volumeSetIdentifier,
        publisherIdentifier,
        dataPreparerIdentifier,
        applicationIdentifier,
        copyrightFileIdentifier,
        abstractFileIdentifier,
        bibliographicFileIdentifier,
        volumeCreationDateAndTime,
        volumeModificationDateAndTime,
        volumeExpirationDateAndTime,
        volumeEffectiveDateAndTime,
        applicationUse,
    })
}

fn makeSupplementary(
    volumeDescriptorVersion: u8,
    volumeFlags: u8,
    systemIdentifier: String,
    volumeIdentifier: String,
    volumeSpaceSize: i64,
    escapeSequences: String,
    volumeSetSize: i64,
    volumeSequenceNumber: i64,
    logicalBlockSize: i64,
    pathTableSize: i64,
    locationOfTypeLPathTable: i64,
    locationOfOptionalTypeLPathTable: i64,
    locationOfTypeMPathTable: i64,
    locationOfOptionalTypeMPathTable: i64,
    rootDirectoryRecord: DirectoryRecord,
    volumeSetIdentifier: String,
    publisherIdentifier: String,
    dataPreparerIdentifier: String,
    applicationIdentifier: String,
    copyrightFileIdentifier: String,
    abstractFileIdentifier: String,
    bibliographicFileIdentifier: String,
    volumeCreationDateAndTime: DateAndTime,
    volumeModificationDateAndTime: DateAndTime,
    volumeExpirationDateAndTime: DateAndTime,
    volumeEffectiveDateAndTime: DateAndTime,
    fileStructureVersion: u8,
    applicationUse: String
) -> TaggedVolumeDescriptor {
    Supplementary {
        volumeDescriptorVersion,
        volumeFlags,
        systemIdentifier,
        volumeIdentifier,
        volumeSpaceSize,
        escapeSequences,
        volumeSetSize,
        volumeSequenceNumber,
        logicalBlockSize,
        pathTableSize,
        locationOfTypeLPathTable,
        locationOfOptionalTypeLPathTable,
        locationOfTypeMPathTable,
        locationOfOptionalTypeMPathTable,
        rootDirectoryRecord,
        volumeSetIdentifier,
        publisherIdentifier,
        dataPreparerIdentifier,
        applicationIdentifier,
        copyrightFileIdentifier,
        abstractFileIdentifier,
        bibliographicFileIdentifier,
        volumeCreationDateAndTime,
        volumeModificationDateAndTime,
        volumeExpirationDateAndTime,
        volumeEffectiveDateAndTime,
        fileStructureVersion,
        applicationUse,
    }
}
fn makeVolumePartition(
    systemIdentifier: String,
    volumePartitionIdentifier: String,
    volumePartitionLocation: i64,
    volumePartitionSize: i64,
    systemUse: String
) -> TaggedVolumeDescriptor {
    VolumePartition {
        systemIdentifier,
        volumePartitionIdentifier,
        volumePartitionLocation,
        volumePartitionSize,
        systemUse,
    }
}
fn makeBoot(
    bootSystemIdentifier: String,
    bootIdentifier: String,
    systemUse: String
) -> TaggedVolumeDescriptor {
    Boot {
        bootSystemIdentifier,
        bootIdentifier,
        systemUse,
    }
}
#[derive(Clone, Debug)]
struct VolumeDescriptor {
  descriptor: TaggedVolumeDescriptor,
}

#[derive(Clone, Debug)]
struct DirectoriesRecursive {
  root: DirectoryTree,
}

#[derive(Clone, Debug)]
struct DirectoryRecord {
  dataLength: i64,
  extendedAttributeRecordLength: u8,
  fileIdentifier: AString,
  fileUnitSize: u8,
  hasPermissions: bool,
  interleaveGapSize: u8,
  isAssociatedFile: bool,
  isDirectory: bool,
  isHidden: bool,
  isMultiExtent: bool,
  isRecord: bool,
  length: u8,
  lengthOfFileIdentifier: u8,
  locationOfExtent: i64,
  recordingDateAndTime: RecordingDateAndTime,
  systemUse: AString,
  volumeSequenceNumber: i64,
}

#[derive(Clone, Debug)]
struct RecordingDateAndTime {
  day: u8,
  gmtOffset: u8,
  hour: u8,
  minute: u8,
  month: u8,
  second: u8,
  year: i64,
}

#[derive(Clone, Debug)]
struct PathTableRecord {
  directoryIdentifier: AString,
  extendedAttributeRecordLength: u8,
  lengthOfDirectoryIdentifier: u8,
  locationOfExtent: i64,
  parentDirectoryNumber: i64,
}

#[derive(Clone, Debug)]
struct A1Chars {
  value: AString,
}

#[derive(Clone, Debug)]
struct D1Chars {
  value: AString,
}

#[derive(Clone, Debug)]
struct DorD1Chars {
  value: AString,
}

#[derive(Clone, Debug)]
struct AorA1Chars {
  value: AString,
}

#[derive(Clone, Debug)]
struct AChars {
  value: AString,
}

#[derive(Clone, Debug)]
struct DChars {
  value: AString,
}

#[derive(Clone, Debug)]
struct DateAndTime {
  day: AString,
  gmtOffset: u8,
  hour: AString,
  hundrethsOfSecond: AString,
  minute: AString,
  month: AString,
  second: AString,
  year: AString,
}

#[derive(Clone, Debug)]
struct Digits {
  value: AString,
}

#[derive(Clone, Debug)]
struct HexBytes {
  value: AString,
}

#[derive(Clone, Debug)]
struct LE_U16 {
  value: i64,
}

#[derive(Clone, Debug)]
struct BE_U16 {
  value: i64,
}

#[derive(Clone, Debug)]
struct LE_U32 {
  value: i64,
}

#[derive(Clone, Debug)]
struct BE_U32 {
  value: i64,
}

#[derive(Clone, Debug)]
struct NULBytes {
}

const LOGICAL_SECTOR_SIZE: i64 = 2048;
#[derive(Clone, Debug)]
struct ISO9660 {
  descriptors: Vec<TaggedVolumeDescriptor>,
  directories: Vec<DirectoryRecords>,
  directoriesRecursive: DirectoryTree,
  logicalBlockSize: i64,
  pathTableLocation: i64,
  pathTableOffset: i64,
  pathTableSize: i64,
  paths: Vec<PathTableRecord>,
  primaryDescriptor: PrimaryVolumeDescriptor,
}

#[derive(Clone, Debug)]
struct VolumeDescriptors {
  values: Vec<TaggedVolumeDescriptor>,
}

#[derive(Clone, Debug)]
struct DirectoryRecords {
  values: Vec<DirectoryRecord>,
}

#[derive(Clone, Debug)]
struct LPathTableRecords {
  values: Vec<PathTableRecord>,
}

#[derive(Clone, Debug)]
struct MPathTableRecords {
  values: Vec<PathTableRecord>,
}

#[derive(Clone, Debug)]
struct ExtendedAttributeRecord {
  applicationUse: AString,
  escapeSequences: AString,
  fileCreationDateAndTime: DateAndTime,
  fileEffectiveDateAndTime: DateAndTime,
  fileExpirationDateAndTime: DateAndTime,
  fileModificationDateAndTime: DateAndTime,
  groupIdentification: i64,
  lengthOfApplicationUse: i64,
  lengthOfEscapeSequences: u8,
  ownerIdentification: i64,
  permissions: i64,
  recordAttributes: u8,
  recordFormat: u8,
  recordLength: i64,
  systemIdentifier: AString,
  systemUse: AString,
}

#[derive(Clone, Debug)]
struct EvenPadByte {
}

#[derive(Clone, Debug)]
struct OddPadByte {
}

#[derive(Clone, Debug)]
struct Digit {
  value: u8,
}

#[derive(Clone, Debug)]
struct BB_U16 {
  value: i64,
}

#[derive(Clone, Debug)]
struct BB_U32 {
  value: i64,
}

#[derive(Clone, Debug)]
struct NUL_BYTE {
}

#[derive(Clone, Debug)]
struct Byte {
  value: u8,
}

fn ISO9660(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, ISO9660)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // VolumeDescriptors@0[16 * LOGICAL_SECTOR_SIZE, EOI]
    left = (16 * LOGICAL_SECTOR_SIZE) as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_VolumeDescriptors_0_m = VolumeDescriptors(input, begin + left, begin + right);
    let (mut nt_VolumeDescriptors_0_ipg_start, mut nt_VolumeDescriptors_0_ipg_end, nt_VolumeDescriptors_0) = match nt_VolumeDescriptors_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_VolumeDescriptors_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_VolumeDescriptors_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_VolumeDescriptors_0_ipg_end);
    }
    nt_VolumeDescriptors_0_ipg_end += left;
    nt_VolumeDescriptors_0_ipg_start += left;
    left = nt_VolumeDescriptors_0_ipg_start;
    right = nt_VolumeDescriptors_0_ipg_end;

    // { descriptors = VolumeDescriptors@0.values }
    let mut self_descriptors = nt_VolumeDescriptors_0.values;

    // { primaryDescriptor = getPrimaryDescriptor(ref(descriptors)) }
    let mut self_primaryDescriptor = getPrimaryDescriptor(&(self_descriptors));

    // { logicalBlockSize = logicalBlockSize(ref(primaryDescriptor)) }
    let mut self_logicalBlockSize = logicalBlockSize(&(self_primaryDescriptor));

    // DirectoriesRecursive@0(logicalBlockSize, rootDirectoryRecord(ref(primaryDescriptor)))[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DirectoriesRecursive_0_m = DirectoriesRecursive(input, begin + left, begin + right, self_logicalBlockSize, rootDirectoryRecord(&(self_primaryDescriptor)));
    let (mut nt_DirectoriesRecursive_0_ipg_start, mut nt_DirectoriesRecursive_0_ipg_end, nt_DirectoriesRecursive_0) = match nt_DirectoriesRecursive_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DirectoriesRecursive_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DirectoriesRecursive_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DirectoriesRecursive_0_ipg_end);
    }
    nt_DirectoriesRecursive_0_ipg_end += left;
    nt_DirectoriesRecursive_0_ipg_start += left;
    left = nt_DirectoriesRecursive_0_ipg_start;
    right = nt_DirectoriesRecursive_0_ipg_end;

    // { directoriesRecursive = DirectoriesRecursive@0.root }
    let mut self_directoriesRecursive = nt_DirectoriesRecursive_0.root;

    // { pathTableLocation = locationOfTypeLPathTable(ref(primaryDescriptor)) }
    let mut self_pathTableLocation = locationOfTypeLPathTable(&(self_primaryDescriptor));

    // { pathTableSize = pathTableSize(ref(primaryDescriptor)) }
    let mut self_pathTableSize = pathTableSize(&(self_primaryDescriptor));

    // { pathTableOffset = logicalBlockSize * pathTableLocation }
    let mut self_pathTableOffset = self_logicalBlockSize * self_pathTableLocation;

    // LPathTableRecords@0(false)[pathTableOffset, pathTableOffset + pathTableSize]
    left = self_pathTableOffset as usize;
    right = (self_pathTableOffset + self_pathTableSize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LPathTableRecords_0_m = LPathTableRecords(input, begin + left, begin + right, false);
    let (mut nt_LPathTableRecords_0_ipg_start, mut nt_LPathTableRecords_0_ipg_end, nt_LPathTableRecords_0) = match nt_LPathTableRecords_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LPathTableRecords_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LPathTableRecords_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LPathTableRecords_0_ipg_end);
    }
    nt_LPathTableRecords_0_ipg_end += left;
    nt_LPathTableRecords_0_ipg_start += left;
    left = nt_LPathTableRecords_0_ipg_start;
    right = nt_LPathTableRecords_0_ipg_end;

    // { paths = LPathTableRecords@0.values }
    let mut self_paths = nt_LPathTableRecords_0.values;

    // for i = 0 to length(ref(paths)) do DirectoryRecords@0(false)[logicalBlockSize * locationOfExtentPT(ref(paths[i])), logicalBlockSize * locationOfExtentPT(ref(paths[i])) + LOGICAL_SECTOR_SIZE]
    let mut nt_DirectoryRecords_0_ipg_start = left;
    let mut nt_DirectoryRecords_0_ipg_end = right;
    let seq_DirectoryRecords_0_start = 0 as usize;
    let loopEnd = length(&(self_paths)) as usize;
    let mut seq_DirectoryRecords_0 = Vec::with_capacity(loopEnd.saturating_sub(seq_DirectoryRecords_0_start));
    for i_i in seq_DirectoryRecords_0_start..loopEnd {
      let left = (self_logicalBlockSize * locationOfExtentPT(&(self_paths[i_i]))) as usize;
      let right = (self_logicalBlockSize * locationOfExtentPT(&(self_paths[i_i])) + LOGICAL_SECTOR_SIZE) as usize;
      if right < left || right > EOI { break '_ipg_alt; }
      let tmp_m = DirectoryRecords(input, begin + left, begin + right, false);
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
      nt_DirectoryRecords_0_ipg_end = tmp_ipg_end;
      nt_DirectoryRecords_0_ipg_start = tmp_ipg_start;
      seq_DirectoryRecords_0.push(tmp);
    }
    left = nt_DirectoryRecords_0_ipg_start;
    right = nt_DirectoryRecords_0_ipg_end;

    // { directories = DirectoryRecords@0.these }
    let mut self_directories = seq_DirectoryRecords_0;

    return Some((self_ipg_start, self_ipg_end, ISO9660 {
      descriptors: self_descriptors,
      directories: self_directories,
      directoriesRecursive: self_directoriesRecursive,
      logicalBlockSize: self_logicalBlockSize,
      pathTableLocation: self_pathTableLocation,
      pathTableOffset: self_pathTableOffset,
      pathTableSize: self_pathTableSize,
      paths: self_paths,
      primaryDescriptor: self_primaryDescriptor,
    }));
  }

  return None;
}

fn VolumeDescriptors(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptors)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat VolumeDescriptor@0[VolumeDescriptor@0.START + LOGICAL_SECTOR_SIZE, VolumeDescriptor@0.START + 2 * LOGICAL_SECTOR_SIZE].descriptor starting on [0, LOGICAL_SECTOR_SIZE] until VolumeDescriptorSetTerminator@0
    left = 0 as usize;
    right = LOGICAL_SECTOR_SIZE as usize;
    let mut self_values = Vec::new();
    let mut nt_VolumeDescriptorSetTerminator_0_ipg_start = right;
    let mut nt_VolumeDescriptorSetTerminator_0_ipg_end = left;
    loop {
      if right < left || right > EOI { break '_ipg_alt; }
      let nt_VolumeDescriptorSetTerminator_0_m = VolumeDescriptorSetTerminator(input, begin + left, begin + right);
      match nt_VolumeDescriptorSetTerminator_0_m {
        None => {}
        Some((nt_VolumeDescriptorSetTerminator_0_ipg_start_, nt_VolumeDescriptorSetTerminator_0_ipg_end_, nt_VolumeDescriptorSetTerminator_0)) => {
          nt_VolumeDescriptorSetTerminator_0_ipg_start = nt_VolumeDescriptorSetTerminator_0_ipg_start_;
          nt_VolumeDescriptorSetTerminator_0_ipg_end = nt_VolumeDescriptorSetTerminator_0_ipg_end_;
          if nt_VolumeDescriptorSetTerminator_0_ipg_end != 0 {
            self_ipg_start = self_ipg_start.min(left + nt_VolumeDescriptorSetTerminator_0_ipg_start);
            self_ipg_end = self_ipg_end.max(left + nt_VolumeDescriptorSetTerminator_0_ipg_end);
          }
          nt_VolumeDescriptorSetTerminator_0_ipg_end += left;
          nt_VolumeDescriptorSetTerminator_0_ipg_start += left;
          right = nt_VolumeDescriptorSetTerminator_0_ipg_end;
          break;
        }
      };
      let nt_VolumeDescriptor_0_m = VolumeDescriptor(input, begin + left, begin + right);
      let (mut nt_VolumeDescriptor_0_ipg_start, mut nt_VolumeDescriptor_0_ipg_end, nt_VolumeDescriptor_0) = match nt_VolumeDescriptor_0_m {
        None => { break '_ipg_alt; }
        Some(p) => p,
      };
      if nt_VolumeDescriptor_0_ipg_end == 0 { panic!("repeat of non-consuming rule: VolumeDescriptor"); }
      self_ipg_start = self_ipg_start.min(left + nt_VolumeDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_VolumeDescriptor_0_ipg_end);
      nt_VolumeDescriptor_0_ipg_end += left;
      nt_VolumeDescriptor_0_ipg_start += left;
      self_values.push(nt_VolumeDescriptor_0.descriptor);
      left = (nt_VolumeDescriptor_0_ipg_start + LOGICAL_SECTOR_SIZE as usize) as usize;
      right = (nt_VolumeDescriptor_0_ipg_start + (2 * LOGICAL_SECTOR_SIZE) as usize) as usize;
    }

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptors {
      values: self_values,
    }));
  }

  return None;
}

fn VolumeDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // PrimaryVolumeDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_PrimaryVolumeDescriptor_0_m = PrimaryVolumeDescriptor(input, begin + left, begin + right);
    let (mut nt_PrimaryVolumeDescriptor_0_ipg_start, mut nt_PrimaryVolumeDescriptor_0_ipg_end, nt_PrimaryVolumeDescriptor_0) = match nt_PrimaryVolumeDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_PrimaryVolumeDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_PrimaryVolumeDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_PrimaryVolumeDescriptor_0_ipg_end);
    }
    nt_PrimaryVolumeDescriptor_0_ipg_end += left;
    nt_PrimaryVolumeDescriptor_0_ipg_start += left;
    left = nt_PrimaryVolumeDescriptor_0_ipg_start;
    right = nt_PrimaryVolumeDescriptor_0_ipg_end;

    // { descriptor = PrimaryVolumeDescriptor@0.descriptor }
    let mut self_descriptor = nt_PrimaryVolumeDescriptor_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // SupplementaryOrEnhancedVolumeDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_SupplementaryOrEnhancedVolumeDescriptor_0_m = SupplementaryOrEnhancedVolumeDescriptor(input, begin + left, begin + right);
    let (mut nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_start, mut nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_end, nt_SupplementaryOrEnhancedVolumeDescriptor_0) = match nt_SupplementaryOrEnhancedVolumeDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_end);
    }
    nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_end += left;
    nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_start += left;
    left = nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_start;
    right = nt_SupplementaryOrEnhancedVolumeDescriptor_0_ipg_end;

    // { descriptor = SupplementaryOrEnhancedVolumeDescriptor@0.descriptor }
    let mut self_descriptor = nt_SupplementaryOrEnhancedVolumeDescriptor_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // VolumePartitionDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_VolumePartitionDescriptor_0_m = VolumePartitionDescriptor(input, begin + left, begin + right);
    let (mut nt_VolumePartitionDescriptor_0_ipg_start, mut nt_VolumePartitionDescriptor_0_ipg_end, nt_VolumePartitionDescriptor_0) = match nt_VolumePartitionDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_VolumePartitionDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_VolumePartitionDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_VolumePartitionDescriptor_0_ipg_end);
    }
    nt_VolumePartitionDescriptor_0_ipg_end += left;
    nt_VolumePartitionDescriptor_0_ipg_start += left;
    left = nt_VolumePartitionDescriptor_0_ipg_start;
    right = nt_VolumePartitionDescriptor_0_ipg_end;

    // { descriptor = VolumePartitionDescriptor@0.descriptor }
    let mut self_descriptor = nt_VolumePartitionDescriptor_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // BootRecord@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BootRecord_0_m = BootRecord(input, begin + left, begin + right);
    let (mut nt_BootRecord_0_ipg_start, mut nt_BootRecord_0_ipg_end, nt_BootRecord_0) = match nt_BootRecord_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BootRecord_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BootRecord_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BootRecord_0_ipg_end);
    }
    nt_BootRecord_0_ipg_end += left;
    nt_BootRecord_0_ipg_start += left;
    left = nt_BootRecord_0_ipg_start;
    right = nt_BootRecord_0_ipg_end;

    // { descriptor = BootRecord@0.descriptor }
    let mut self_descriptor = nt_BootRecord_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // VolumeDescriptorSetTerminator@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_VolumeDescriptorSetTerminator_0_m = VolumeDescriptorSetTerminator(input, begin + left, begin + right);
    let (mut nt_VolumeDescriptorSetTerminator_0_ipg_start, mut nt_VolumeDescriptorSetTerminator_0_ipg_end, nt_VolumeDescriptorSetTerminator_0) = match nt_VolumeDescriptorSetTerminator_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_VolumeDescriptorSetTerminator_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_VolumeDescriptorSetTerminator_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_VolumeDescriptorSetTerminator_0_ipg_end);
    }
    nt_VolumeDescriptorSetTerminator_0_ipg_end += left;
    nt_VolumeDescriptorSetTerminator_0_ipg_start += left;
    left = nt_VolumeDescriptorSetTerminator_0_ipg_start;
    right = nt_VolumeDescriptorSetTerminator_0_ipg_end;

    // { descriptor = VolumeDescriptorSetTerminator@0.descriptor }
    let mut self_descriptor = nt_VolumeDescriptorSetTerminator_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // UnknownVolumeDescriptor@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_UnknownVolumeDescriptor_0_m = UnknownVolumeDescriptor(input, begin + left, begin + right);
    let (mut nt_UnknownVolumeDescriptor_0_ipg_start, mut nt_UnknownVolumeDescriptor_0_ipg_end, nt_UnknownVolumeDescriptor_0) = match nt_UnknownVolumeDescriptor_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_UnknownVolumeDescriptor_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_UnknownVolumeDescriptor_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_UnknownVolumeDescriptor_0_ipg_end);
    }
    nt_UnknownVolumeDescriptor_0_ipg_end += left;
    nt_UnknownVolumeDescriptor_0_ipg_start += left;
    left = nt_UnknownVolumeDescriptor_0_ipg_start;
    right = nt_UnknownVolumeDescriptor_0_ipg_end;

    // { descriptor = UnknownVolumeDescriptor@0.descriptor }
    let mut self_descriptor = nt_UnknownVolumeDescriptor_0.descriptor;

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn PrimaryVolumeDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x01CD001\x01\x00"[0, 8]
    left = 0 as usize;
    right = 8 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[1, 67, 68, 48, 48, 49, 1, 0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 8;
    self_ipg_end = self_ipg_end.max(right);

    // AChars@0[8, 40]
    left = 8 as usize;
    right = 40 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_0_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_0_ipg_start, mut nt_AChars_0_ipg_end, nt_AChars_0) = match nt_AChars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_0_ipg_end);
    }
    nt_AChars_0_ipg_end += left;
    nt_AChars_0_ipg_start += left;
    left = nt_AChars_0_ipg_start;
    right = nt_AChars_0_ipg_end;

    // { systemIdentifier = AChars@0.value }
    let mut self_systemIdentifier = nt_AChars_0.value;

    // DChars@0[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0_ipg_end as usize;
    right = (nt_AChars_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_0_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_0_ipg_start, mut nt_DChars_0_ipg_end, nt_DChars_0) = match nt_DChars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_0_ipg_end);
    }
    nt_DChars_0_ipg_end += left;
    nt_DChars_0_ipg_start += left;
    left = nt_DChars_0_ipg_start;
    right = nt_DChars_0_ipg_end;

    // { volumeIdentifier = DChars@0.value }
    let mut self_volumeIdentifier = nt_DChars_0.value;

    // NULBytes@0[DChars@0.END, DChars@0.END + 8]
    left = nt_DChars_0_ipg_end as usize;
    right = (nt_DChars_0_ipg_end + 8 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_0_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_0_ipg_start, mut nt_NULBytes_0_ipg_end, nt_NULBytes_0) = match nt_NULBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_0_ipg_end);
    }
    nt_NULBytes_0_ipg_end += left;
    nt_NULBytes_0_ipg_start += left;
    left = nt_NULBytes_0_ipg_start;
    right = nt_NULBytes_0_ipg_end;

    // BB_U32@0[NULBytes@0.END, EOI]
    left = nt_NULBytes_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_0_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_0_ipg_start, mut nt_BB_U32_0_ipg_end, nt_BB_U32_0) = match nt_BB_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_0_ipg_end);
    }
    nt_BB_U32_0_ipg_end += left;
    nt_BB_U32_0_ipg_start += left;
    left = nt_BB_U32_0_ipg_start;
    right = nt_BB_U32_0_ipg_end;

    // { volumeSpaceSize = BB_U32@0.value }
    let mut self_volumeSpaceSize = nt_BB_U32_0.value;

    // NULBytes@1[BB_U32@0.END, BB_U32@0.END + 32]
    left = nt_BB_U32_0_ipg_end as usize;
    right = (nt_BB_U32_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_1_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_1_ipg_start, mut nt_NULBytes_1_ipg_end, nt_NULBytes_1) = match nt_NULBytes_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_1_ipg_end);
    }
    nt_NULBytes_1_ipg_end += left;
    nt_NULBytes_1_ipg_start += left;
    left = nt_NULBytes_1_ipg_start;
    right = nt_NULBytes_1_ipg_end;

    // BB_U16@0[NULBytes@1.END, NULBytes@1.END + 4]
    left = nt_NULBytes_1_ipg_end as usize;
    right = (nt_NULBytes_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_0_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_0_ipg_start, mut nt_BB_U16_0_ipg_end, nt_BB_U16_0) = match nt_BB_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_0_ipg_end);
    }
    nt_BB_U16_0_ipg_end += left;
    nt_BB_U16_0_ipg_start += left;
    left = nt_BB_U16_0_ipg_start;
    right = nt_BB_U16_0_ipg_end;

    // { volumeSetSize = BB_U16@0.value }
    let mut self_volumeSetSize = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, BB_U16@0.END + 4]
    left = nt_BB_U16_0_ipg_end as usize;
    right = (nt_BB_U16_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_1_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_1_ipg_start, mut nt_BB_U16_1_ipg_end, nt_BB_U16_1) = match nt_BB_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_1_ipg_end);
    }
    nt_BB_U16_1_ipg_end += left;
    nt_BB_U16_1_ipg_start += left;
    left = nt_BB_U16_1_ipg_start;
    right = nt_BB_U16_1_ipg_end;

    // { volumeSequenceNumber = BB_U16@1.value }
    let mut self_volumeSequenceNumber = nt_BB_U16_1.value;

    // BB_U16@2[BB_U16@1.END, BB_U16@1.END + 4]
    left = nt_BB_U16_1_ipg_end as usize;
    right = (nt_BB_U16_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_2_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_2_ipg_start, mut nt_BB_U16_2_ipg_end, nt_BB_U16_2) = match nt_BB_U16_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_2_ipg_end);
    }
    nt_BB_U16_2_ipg_end += left;
    nt_BB_U16_2_ipg_start += left;
    left = nt_BB_U16_2_ipg_start;
    right = nt_BB_U16_2_ipg_end;

    // { logicalBlockSize = BB_U16@2.value }
    let mut self_logicalBlockSize = nt_BB_U16_2.value;

    // BB_U32@1[BB_U16@2.END, BB_U16@2.END + 8]
    left = nt_BB_U16_2_ipg_end as usize;
    right = (nt_BB_U16_2_ipg_end + 8 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_1_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_1_ipg_start, mut nt_BB_U32_1_ipg_end, nt_BB_U32_1) = match nt_BB_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_1_ipg_end);
    }
    nt_BB_U32_1_ipg_end += left;
    nt_BB_U32_1_ipg_start += left;
    left = nt_BB_U32_1_ipg_start;
    right = nt_BB_U32_1_ipg_end;

    // { pathTableSize = BB_U32@1.value }
    let mut self_pathTableSize = nt_BB_U32_1.value;

    // LE_U32@0[BB_U32@1.END, BB_U32@1.END + 4]
    left = nt_BB_U32_1_ipg_end as usize;
    right = (nt_BB_U32_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_0_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_0_ipg_start, mut nt_LE_U32_0_ipg_end, nt_LE_U32_0) = match nt_LE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_0_ipg_end);
    }
    nt_LE_U32_0_ipg_end += left;
    nt_LE_U32_0_ipg_start += left;
    left = nt_LE_U32_0_ipg_start;
    right = nt_LE_U32_0_ipg_end;

    // { locationOfTypeLPathTable = LE_U32@0.value }
    let mut self_locationOfTypeLPathTable = nt_LE_U32_0.value;

    // LE_U32@1[LE_U32@0.END, LE_U32@0.END + 4]
    left = nt_LE_U32_0_ipg_end as usize;
    right = (nt_LE_U32_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_1_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_1_ipg_start, mut nt_LE_U32_1_ipg_end, nt_LE_U32_1) = match nt_LE_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_1_ipg_end);
    }
    nt_LE_U32_1_ipg_end += left;
    nt_LE_U32_1_ipg_start += left;
    left = nt_LE_U32_1_ipg_start;
    right = nt_LE_U32_1_ipg_end;

    // { locationOfOptionalTypeLPathTable = LE_U32@1.value }
    let mut self_locationOfOptionalTypeLPathTable = nt_LE_U32_1.value;

    // BE_U32@0[LE_U32@1.END, LE_U32@1.END + 4]
    left = nt_LE_U32_1_ipg_end as usize;
    right = (nt_LE_U32_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_0_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_0_ipg_start, mut nt_BE_U32_0_ipg_end, nt_BE_U32_0) = match nt_BE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_0_ipg_end);
    }
    nt_BE_U32_0_ipg_end += left;
    nt_BE_U32_0_ipg_start += left;
    left = nt_BE_U32_0_ipg_start;
    right = nt_BE_U32_0_ipg_end;

    // { locationOfTypeMPathTable = BE_U32@0.value }
    let mut self_locationOfTypeMPathTable = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, BE_U32@0.END + 4]
    left = nt_BE_U32_0_ipg_end as usize;
    right = (nt_BE_U32_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_1_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_1_ipg_start, mut nt_BE_U32_1_ipg_end, nt_BE_U32_1) = match nt_BE_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_1_ipg_end);
    }
    nt_BE_U32_1_ipg_end += left;
    nt_BE_U32_1_ipg_start += left;
    left = nt_BE_U32_1_ipg_start;
    right = nt_BE_U32_1_ipg_end;

    // { locationOfOptionalTypeMPathTable = BE_U32@1.value }
    let mut self_locationOfOptionalTypeMPathTable = nt_BE_U32_1.value;

    // DirectoryRecord@0(false)[BE_U32@1.END, BE_U32@1.END + 34]
    left = nt_BE_U32_1_ipg_end as usize;
    right = (nt_BE_U32_1_ipg_end + 34 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DirectoryRecord_0_m = DirectoryRecord(input, begin + left, begin + right, false);
    let (mut nt_DirectoryRecord_0_ipg_start, mut nt_DirectoryRecord_0_ipg_end, nt_DirectoryRecord_0) = match nt_DirectoryRecord_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DirectoryRecord_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DirectoryRecord_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DirectoryRecord_0_ipg_end);
    }
    nt_DirectoryRecord_0_ipg_end += left;
    nt_DirectoryRecord_0_ipg_start += left;
    left = nt_DirectoryRecord_0_ipg_start;
    right = nt_DirectoryRecord_0_ipg_end;

    // { rootDirectoryRecord = DirectoryRecord@0.this }
    let mut self_rootDirectoryRecord = nt_DirectoryRecord_0;

    // DChars@1[190, 318]
    left = 190 as usize;
    right = 318 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_1_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_1_ipg_start, mut nt_DChars_1_ipg_end, nt_DChars_1) = match nt_DChars_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_1_ipg_end);
    }
    nt_DChars_1_ipg_end += left;
    nt_DChars_1_ipg_start += left;
    left = nt_DChars_1_ipg_start;
    right = nt_DChars_1_ipg_end;

    // { volumeSetIdentifier = DChars@1.value }
    let mut self_volumeSetIdentifier = nt_DChars_1.value;

    // AChars@1[DChars@1.END, DChars@1.END + 128]
    left = nt_DChars_1_ipg_end as usize;
    right = (nt_DChars_1_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_1_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_1_ipg_start, mut nt_AChars_1_ipg_end, nt_AChars_1) = match nt_AChars_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_1_ipg_end);
    }
    nt_AChars_1_ipg_end += left;
    nt_AChars_1_ipg_start += left;
    left = nt_AChars_1_ipg_start;
    right = nt_AChars_1_ipg_end;

    // { publisherIdentifier = AChars@1.value }
    let mut self_publisherIdentifier = nt_AChars_1.value;

    // AChars@2[AChars@1.END, AChars@1.END + 128]
    left = nt_AChars_1_ipg_end as usize;
    right = (nt_AChars_1_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_2_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_2_ipg_start, mut nt_AChars_2_ipg_end, nt_AChars_2) = match nt_AChars_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_2_ipg_end);
    }
    nt_AChars_2_ipg_end += left;
    nt_AChars_2_ipg_start += left;
    left = nt_AChars_2_ipg_start;
    right = nt_AChars_2_ipg_end;

    // { dataPreparerIdentifier = AChars@2.value }
    let mut self_dataPreparerIdentifier = nt_AChars_2.value;

    // AChars@3[AChars@2.END, AChars@2.END + 128]
    left = nt_AChars_2_ipg_end as usize;
    right = (nt_AChars_2_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_3_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_3_ipg_start, mut nt_AChars_3_ipg_end, nt_AChars_3) = match nt_AChars_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_3_ipg_end);
    }
    nt_AChars_3_ipg_end += left;
    nt_AChars_3_ipg_start += left;
    left = nt_AChars_3_ipg_start;
    right = nt_AChars_3_ipg_end;

    // { applicationIdentifier = AChars@3.value }
    let mut self_applicationIdentifier = nt_AChars_3.value;

    // DChars@2[AChars@3.END, AChars@3.END + 37]
    left = nt_AChars_3_ipg_end as usize;
    right = (nt_AChars_3_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_2_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_2_ipg_start, mut nt_DChars_2_ipg_end, nt_DChars_2) = match nt_DChars_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_2_ipg_end);
    }
    nt_DChars_2_ipg_end += left;
    nt_DChars_2_ipg_start += left;
    left = nt_DChars_2_ipg_start;
    right = nt_DChars_2_ipg_end;

    // { copyrightFileIdentifier = DChars@2.value }
    let mut self_copyrightFileIdentifier = nt_DChars_2.value;

    // DChars@3[DChars@2.END, DChars@2.END + 37]
    left = nt_DChars_2_ipg_end as usize;
    right = (nt_DChars_2_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_3_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_3_ipg_start, mut nt_DChars_3_ipg_end, nt_DChars_3) = match nt_DChars_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_3_ipg_end);
    }
    nt_DChars_3_ipg_end += left;
    nt_DChars_3_ipg_start += left;
    left = nt_DChars_3_ipg_start;
    right = nt_DChars_3_ipg_end;

    // { abstractFileIdentifier = DChars@3.value }
    let mut self_abstractFileIdentifier = nt_DChars_3.value;

    // DChars@4[DChars@3.END, DChars@3.END + 37]
    left = nt_DChars_3_ipg_end as usize;
    right = (nt_DChars_3_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_4_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_4_ipg_start, mut nt_DChars_4_ipg_end, nt_DChars_4) = match nt_DChars_4_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_4_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_4_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_4_ipg_end);
    }
    nt_DChars_4_ipg_end += left;
    nt_DChars_4_ipg_start += left;
    left = nt_DChars_4_ipg_start;
    right = nt_DChars_4_ipg_end;

    // { bibliographicFileIdentifier = DChars@4.value }
    let mut self_bibliographicFileIdentifier = nt_DChars_4.value;

    // DateAndTime@0[DChars@4.END, DChars@4.END + 17]
    left = nt_DChars_4_ipg_end as usize;
    right = (nt_DChars_4_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_0_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_0_ipg_start, mut nt_DateAndTime_0_ipg_end, nt_DateAndTime_0) = match nt_DateAndTime_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_0_ipg_end);
    }
    nt_DateAndTime_0_ipg_end += left;
    nt_DateAndTime_0_ipg_start += left;
    left = nt_DateAndTime_0_ipg_start;
    right = nt_DateAndTime_0_ipg_end;

    // { volumeCreationDateAndTime = DateAndTime@0.this }
    let mut self_volumeCreationDateAndTime = nt_DateAndTime_0;

    // DateAndTime@1[DateAndTime@0.END, DateAndTime@0.END + 17]
    left = nt_DateAndTime_0_ipg_end as usize;
    right = (nt_DateAndTime_0_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_1_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_1_ipg_start, mut nt_DateAndTime_1_ipg_end, nt_DateAndTime_1) = match nt_DateAndTime_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_1_ipg_end);
    }
    nt_DateAndTime_1_ipg_end += left;
    nt_DateAndTime_1_ipg_start += left;
    left = nt_DateAndTime_1_ipg_start;
    right = nt_DateAndTime_1_ipg_end;

    // { volumeModificationDateAndTime = DateAndTime@1.this }
    let mut self_volumeModificationDateAndTime = nt_DateAndTime_1;

    // DateAndTime@2[DateAndTime@1.END, DateAndTime@1.END + 17]
    left = nt_DateAndTime_1_ipg_end as usize;
    right = (nt_DateAndTime_1_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_2_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_2_ipg_start, mut nt_DateAndTime_2_ipg_end, nt_DateAndTime_2) = match nt_DateAndTime_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_2_ipg_end);
    }
    nt_DateAndTime_2_ipg_end += left;
    nt_DateAndTime_2_ipg_start += left;
    left = nt_DateAndTime_2_ipg_start;
    right = nt_DateAndTime_2_ipg_end;

    // { volumeExpirationDateAndTime = DateAndTime@2.this }
    let mut self_volumeExpirationDateAndTime = nt_DateAndTime_2;

    // DateAndTime@3[DateAndTime@2.END, DateAndTime@2.END + 17]
    left = nt_DateAndTime_2_ipg_end as usize;
    right = (nt_DateAndTime_2_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_3_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_3_ipg_start, mut nt_DateAndTime_3_ipg_end, nt_DateAndTime_3) = match nt_DateAndTime_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_3_ipg_end);
    }
    nt_DateAndTime_3_ipg_end += left;
    nt_DateAndTime_3_ipg_start += left;
    left = nt_DateAndTime_3_ipg_start;
    right = nt_DateAndTime_3_ipg_end;

    // { volumeEffectiveDateAndTime = DateAndTime@3.this }
    let mut self_volumeEffectiveDateAndTime = nt_DateAndTime_3;

    // "\x01"[DateAndTime@3.END, DateAndTime@3.END + 1]
    left = nt_DateAndTime_3_ipg_end as usize;
    right = (nt_DateAndTime_3_ipg_end + 1 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // "\x00"[DateAndTime@3.END + 1, DateAndTime@3.END + 2]
    left = (nt_DateAndTime_3_ipg_end + 1 as usize) as usize;
    right = (nt_DateAndTime_3_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // HexBytes@0[DateAndTime@3.END + 2, DateAndTime@3.END + 514]
    left = (nt_DateAndTime_3_ipg_end + 2 as usize) as usize;
    right = (nt_DateAndTime_3_ipg_end + 514 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { applicationUse = HexBytes@0.value }
    let mut self_applicationUse = nt_HexBytes_0.value;

    // NULBytes@2[HexBytes@0.END, EOI]
    left = nt_HexBytes_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_2_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_2_ipg_start, mut nt_NULBytes_2_ipg_end, nt_NULBytes_2) = match nt_NULBytes_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_2_ipg_end);
    }
    nt_NULBytes_2_ipg_end += left;
    nt_NULBytes_2_ipg_start += left;
    left = nt_NULBytes_2_ipg_start;
    right = nt_NULBytes_2_ipg_end;

    // { descriptor = makePrimaryVolumeDescriptor(systemIdentifier, volumeIdentifier, volumeSpaceSize, volumeSetSize, volumeSequenceNumber, logicalBlockSize, pathTableSize, locationOfTypeLPathTable, locationOfOptionalTypeLPathTable, locationOfTypeMPathTable, locationOfOptionalTypeMPathTable, rootDirectoryRecord, volumeSetIdentifier, publisherIdentifier, dataPreparerIdentifier, applicationIdentifier, copyrightFileIdentifier, abstractFileIdentifier, bibliographicFileIdentifier, volumeCreationDateAndTime, volumeModificationDateAndTime, volumeExpirationDateAndTime, volumeEffectiveDateAndTime, applicationUse) }
    let mut self_descriptor = makePrimaryVolumeDescriptor(self_systemIdentifier, self_volumeIdentifier, self_volumeSpaceSize, self_volumeSetSize, self_volumeSequenceNumber, self_logicalBlockSize, self_pathTableSize, self_locationOfTypeLPathTable, self_locationOfOptionalTypeLPathTable, self_locationOfTypeMPathTable, self_locationOfOptionalTypeMPathTable, self_rootDirectoryRecord, self_volumeSetIdentifier, self_publisherIdentifier, self_dataPreparerIdentifier, self_applicationIdentifier, self_copyrightFileIdentifier, self_abstractFileIdentifier, self_bibliographicFileIdentifier, self_volumeCreationDateAndTime, self_volumeModificationDateAndTime, self_volumeExpirationDateAndTime, self_volumeEffectiveDateAndTime, self_applicationUse);

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn SupplementaryOrEnhancedVolumeDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x02CD001"[0, 6]
    left = 0 as usize;
    right = 6 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[2, 67, 68, 48, 48, 49]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 6;
    self_ipg_end = self_ipg_end.max(right);

    // { volumeDescriptorVersion = .[6] }
    left = 6 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_volumeDescriptorVersion = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // ?[ volumeDescriptorVersion == 1 || volumeDescriptorVersion == 2 ]
    if !(self_volumeDescriptorVersion == 1 || self_volumeDescriptorVersion == 2) { break '_ipg_alt; }

    // { volumeFlags = .[7] }
    left = 7 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_volumeFlags = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // A1Chars@0[8, 40]
    left = 8 as usize;
    right = 40 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_A1Chars_0_m = A1Chars(input, begin + left, begin + right);
    let (mut nt_A1Chars_0_ipg_start, mut nt_A1Chars_0_ipg_end, nt_A1Chars_0) = match nt_A1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_A1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_A1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_A1Chars_0_ipg_end);
    }
    nt_A1Chars_0_ipg_end += left;
    nt_A1Chars_0_ipg_start += left;
    left = nt_A1Chars_0_ipg_start;
    right = nt_A1Chars_0_ipg_end;

    // { systemIdentifier = A1Chars@0.value }
    let mut self_systemIdentifier = nt_A1Chars_0.value;

    // D1Chars@0[A1Chars@0.END, A1Chars@0.END + 32]
    left = nt_A1Chars_0_ipg_end as usize;
    right = (nt_A1Chars_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_D1Chars_0_m = D1Chars(input, begin + left, begin + right);
    let (mut nt_D1Chars_0_ipg_start, mut nt_D1Chars_0_ipg_end, nt_D1Chars_0) = match nt_D1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_D1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_D1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_D1Chars_0_ipg_end);
    }
    nt_D1Chars_0_ipg_end += left;
    nt_D1Chars_0_ipg_start += left;
    left = nt_D1Chars_0_ipg_start;
    right = nt_D1Chars_0_ipg_end;

    // { volumeIdentifier = D1Chars@0.value }
    let mut self_volumeIdentifier = nt_D1Chars_0.value;

    // NULBytes@0[D1Chars@0.END, D1Chars@0.END + 8]
    left = nt_D1Chars_0_ipg_end as usize;
    right = (nt_D1Chars_0_ipg_end + 8 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_0_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_0_ipg_start, mut nt_NULBytes_0_ipg_end, nt_NULBytes_0) = match nt_NULBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_0_ipg_end);
    }
    nt_NULBytes_0_ipg_end += left;
    nt_NULBytes_0_ipg_start += left;
    left = nt_NULBytes_0_ipg_start;
    right = nt_NULBytes_0_ipg_end;

    // BB_U32@0[NULBytes@0.END, EOI]
    left = nt_NULBytes_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_0_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_0_ipg_start, mut nt_BB_U32_0_ipg_end, nt_BB_U32_0) = match nt_BB_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_0_ipg_end);
    }
    nt_BB_U32_0_ipg_end += left;
    nt_BB_U32_0_ipg_start += left;
    left = nt_BB_U32_0_ipg_start;
    right = nt_BB_U32_0_ipg_end;

    // { volumeSpaceSize = BB_U32@0.value }
    let mut self_volumeSpaceSize = nt_BB_U32_0.value;

    // HexBytes@0[BB_U32@0.END, BB_U32@0.END + 32]
    left = nt_BB_U32_0_ipg_end as usize;
    right = (nt_BB_U32_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { escapeSequences = HexBytes@0.value }
    let mut self_escapeSequences = nt_HexBytes_0.value;

    // BB_U16@0[HexBytes@0.END, HexBytes@0.END + 4]
    left = nt_HexBytes_0_ipg_end as usize;
    right = (nt_HexBytes_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_0_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_0_ipg_start, mut nt_BB_U16_0_ipg_end, nt_BB_U16_0) = match nt_BB_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_0_ipg_end);
    }
    nt_BB_U16_0_ipg_end += left;
    nt_BB_U16_0_ipg_start += left;
    left = nt_BB_U16_0_ipg_start;
    right = nt_BB_U16_0_ipg_end;

    // { volumeSetSize = BB_U16@0.value }
    let mut self_volumeSetSize = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, BB_U16@0.END + 4]
    left = nt_BB_U16_0_ipg_end as usize;
    right = (nt_BB_U16_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_1_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_1_ipg_start, mut nt_BB_U16_1_ipg_end, nt_BB_U16_1) = match nt_BB_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_1_ipg_end);
    }
    nt_BB_U16_1_ipg_end += left;
    nt_BB_U16_1_ipg_start += left;
    left = nt_BB_U16_1_ipg_start;
    right = nt_BB_U16_1_ipg_end;

    // { volumeSequenceNumber = BB_U16@1.value }
    let mut self_volumeSequenceNumber = nt_BB_U16_1.value;

    // BB_U16@2[BB_U16@1.END, BB_U16@1.END + 4]
    left = nt_BB_U16_1_ipg_end as usize;
    right = (nt_BB_U16_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_2_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_2_ipg_start, mut nt_BB_U16_2_ipg_end, nt_BB_U16_2) = match nt_BB_U16_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_2_ipg_end);
    }
    nt_BB_U16_2_ipg_end += left;
    nt_BB_U16_2_ipg_start += left;
    left = nt_BB_U16_2_ipg_start;
    right = nt_BB_U16_2_ipg_end;

    // { logicalBlockSize = BB_U16@2.value }
    let mut self_logicalBlockSize = nt_BB_U16_2.value;

    // BB_U32@1[BB_U16@2.END, BB_U16@2.END + 8]
    left = nt_BB_U16_2_ipg_end as usize;
    right = (nt_BB_U16_2_ipg_end + 8 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_1_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_1_ipg_start, mut nt_BB_U32_1_ipg_end, nt_BB_U32_1) = match nt_BB_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_1_ipg_end);
    }
    nt_BB_U32_1_ipg_end += left;
    nt_BB_U32_1_ipg_start += left;
    left = nt_BB_U32_1_ipg_start;
    right = nt_BB_U32_1_ipg_end;

    // { pathTableSize = BB_U32@1.value }
    let mut self_pathTableSize = nt_BB_U32_1.value;

    // LE_U32@0[BB_U32@1.END, BB_U32@1.END + 4]
    left = nt_BB_U32_1_ipg_end as usize;
    right = (nt_BB_U32_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_0_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_0_ipg_start, mut nt_LE_U32_0_ipg_end, nt_LE_U32_0) = match nt_LE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_0_ipg_end);
    }
    nt_LE_U32_0_ipg_end += left;
    nt_LE_U32_0_ipg_start += left;
    left = nt_LE_U32_0_ipg_start;
    right = nt_LE_U32_0_ipg_end;

    // { locationOfTypeLPathTable = LE_U32@0.value }
    let mut self_locationOfTypeLPathTable = nt_LE_U32_0.value;

    // LE_U32@1[LE_U32@0.END, LE_U32@0.END + 4]
    left = nt_LE_U32_0_ipg_end as usize;
    right = (nt_LE_U32_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_1_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_1_ipg_start, mut nt_LE_U32_1_ipg_end, nt_LE_U32_1) = match nt_LE_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_1_ipg_end);
    }
    nt_LE_U32_1_ipg_end += left;
    nt_LE_U32_1_ipg_start += left;
    left = nt_LE_U32_1_ipg_start;
    right = nt_LE_U32_1_ipg_end;

    // { locationOfOptionalTypeLPathTable = LE_U32@1.value }
    let mut self_locationOfOptionalTypeLPathTable = nt_LE_U32_1.value;

    // BE_U32@0[LE_U32@1.END, LE_U32@1.END + 4]
    left = nt_LE_U32_1_ipg_end as usize;
    right = (nt_LE_U32_1_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_0_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_0_ipg_start, mut nt_BE_U32_0_ipg_end, nt_BE_U32_0) = match nt_BE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_0_ipg_end);
    }
    nt_BE_U32_0_ipg_end += left;
    nt_BE_U32_0_ipg_start += left;
    left = nt_BE_U32_0_ipg_start;
    right = nt_BE_U32_0_ipg_end;

    // { locationOfTypeMPathTable = BE_U32@0.value }
    let mut self_locationOfTypeMPathTable = nt_BE_U32_0.value;

    // BE_U32@1[BE_U32@0.END, BE_U32@0.END + 4]
    left = nt_BE_U32_0_ipg_end as usize;
    right = (nt_BE_U32_0_ipg_end + 4 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_1_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_1_ipg_start, mut nt_BE_U32_1_ipg_end, nt_BE_U32_1) = match nt_BE_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_1_ipg_end);
    }
    nt_BE_U32_1_ipg_end += left;
    nt_BE_U32_1_ipg_start += left;
    left = nt_BE_U32_1_ipg_start;
    right = nt_BE_U32_1_ipg_end;

    // { locationOfOptionalTypeMPathTable = BE_U32@1.value }
    let mut self_locationOfOptionalTypeMPathTable = nt_BE_U32_1.value;

    // DirectoryRecord@0(true)[BE_U32@1.END, BE_U32@1.END + 34]
    left = nt_BE_U32_1_ipg_end as usize;
    right = (nt_BE_U32_1_ipg_end + 34 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DirectoryRecord_0_m = DirectoryRecord(input, begin + left, begin + right, true);
    let (mut nt_DirectoryRecord_0_ipg_start, mut nt_DirectoryRecord_0_ipg_end, nt_DirectoryRecord_0) = match nt_DirectoryRecord_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DirectoryRecord_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DirectoryRecord_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DirectoryRecord_0_ipg_end);
    }
    nt_DirectoryRecord_0_ipg_end += left;
    nt_DirectoryRecord_0_ipg_start += left;
    left = nt_DirectoryRecord_0_ipg_start;
    right = nt_DirectoryRecord_0_ipg_end;

    // { rootDirectoryRecord = DirectoryRecord@0.this }
    let mut self_rootDirectoryRecord = nt_DirectoryRecord_0;

    // D1Chars@1[190, 318]
    left = 190 as usize;
    right = 318 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_D1Chars_1_m = D1Chars(input, begin + left, begin + right);
    let (mut nt_D1Chars_1_ipg_start, mut nt_D1Chars_1_ipg_end, nt_D1Chars_1) = match nt_D1Chars_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_D1Chars_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_D1Chars_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_D1Chars_1_ipg_end);
    }
    nt_D1Chars_1_ipg_end += left;
    nt_D1Chars_1_ipg_start += left;
    left = nt_D1Chars_1_ipg_start;
    right = nt_D1Chars_1_ipg_end;

    // { volumeSetIdentifier = D1Chars@1.value }
    let mut self_volumeSetIdentifier = nt_D1Chars_1.value;

    // A1Chars@1[D1Chars@1.END, D1Chars@1.END + 128]
    left = nt_D1Chars_1_ipg_end as usize;
    right = (nt_D1Chars_1_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_A1Chars_1_m = A1Chars(input, begin + left, begin + right);
    let (mut nt_A1Chars_1_ipg_start, mut nt_A1Chars_1_ipg_end, nt_A1Chars_1) = match nt_A1Chars_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_A1Chars_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_A1Chars_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_A1Chars_1_ipg_end);
    }
    nt_A1Chars_1_ipg_end += left;
    nt_A1Chars_1_ipg_start += left;
    left = nt_A1Chars_1_ipg_start;
    right = nt_A1Chars_1_ipg_end;

    // { publisherIdentifier = A1Chars@1.value }
    let mut self_publisherIdentifier = nt_A1Chars_1.value;

    // A1Chars@2[A1Chars@1.END, A1Chars@1.END + 128]
    left = nt_A1Chars_1_ipg_end as usize;
    right = (nt_A1Chars_1_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_A1Chars_2_m = A1Chars(input, begin + left, begin + right);
    let (mut nt_A1Chars_2_ipg_start, mut nt_A1Chars_2_ipg_end, nt_A1Chars_2) = match nt_A1Chars_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_A1Chars_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_A1Chars_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_A1Chars_2_ipg_end);
    }
    nt_A1Chars_2_ipg_end += left;
    nt_A1Chars_2_ipg_start += left;
    left = nt_A1Chars_2_ipg_start;
    right = nt_A1Chars_2_ipg_end;

    // { dataPreparerIdentifier = A1Chars@2.value }
    let mut self_dataPreparerIdentifier = nt_A1Chars_2.value;

    // A1Chars@3[A1Chars@2.END, A1Chars@2.END + 128]
    left = nt_A1Chars_2_ipg_end as usize;
    right = (nt_A1Chars_2_ipg_end + 128 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_A1Chars_3_m = A1Chars(input, begin + left, begin + right);
    let (mut nt_A1Chars_3_ipg_start, mut nt_A1Chars_3_ipg_end, nt_A1Chars_3) = match nt_A1Chars_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_A1Chars_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_A1Chars_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_A1Chars_3_ipg_end);
    }
    nt_A1Chars_3_ipg_end += left;
    nt_A1Chars_3_ipg_start += left;
    left = nt_A1Chars_3_ipg_start;
    right = nt_A1Chars_3_ipg_end;

    // { applicationIdentifier = A1Chars@3.value }
    let mut self_applicationIdentifier = nt_A1Chars_3.value;

    // D1Chars@2[A1Chars@3.END, A1Chars@3.END + 37]
    left = nt_A1Chars_3_ipg_end as usize;
    right = (nt_A1Chars_3_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_D1Chars_2_m = D1Chars(input, begin + left, begin + right);
    let (mut nt_D1Chars_2_ipg_start, mut nt_D1Chars_2_ipg_end, nt_D1Chars_2) = match nt_D1Chars_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_D1Chars_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_D1Chars_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_D1Chars_2_ipg_end);
    }
    nt_D1Chars_2_ipg_end += left;
    nt_D1Chars_2_ipg_start += left;
    left = nt_D1Chars_2_ipg_start;
    right = nt_D1Chars_2_ipg_end;

    // { copyrightFileIdentifier = D1Chars@2.value }
    let mut self_copyrightFileIdentifier = nt_D1Chars_2.value;

    // D1Chars@3[D1Chars@2.END, D1Chars@2.END + 37]
    left = nt_D1Chars_2_ipg_end as usize;
    right = (nt_D1Chars_2_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_D1Chars_3_m = D1Chars(input, begin + left, begin + right);
    let (mut nt_D1Chars_3_ipg_start, mut nt_D1Chars_3_ipg_end, nt_D1Chars_3) = match nt_D1Chars_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_D1Chars_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_D1Chars_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_D1Chars_3_ipg_end);
    }
    nt_D1Chars_3_ipg_end += left;
    nt_D1Chars_3_ipg_start += left;
    left = nt_D1Chars_3_ipg_start;
    right = nt_D1Chars_3_ipg_end;

    // { abstractFileIdentifier = D1Chars@3.value }
    let mut self_abstractFileIdentifier = nt_D1Chars_3.value;

    // D1Chars@4[D1Chars@3.END, D1Chars@3.END + 37]
    left = nt_D1Chars_3_ipg_end as usize;
    right = (nt_D1Chars_3_ipg_end + 37 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_D1Chars_4_m = D1Chars(input, begin + left, begin + right);
    let (mut nt_D1Chars_4_ipg_start, mut nt_D1Chars_4_ipg_end, nt_D1Chars_4) = match nt_D1Chars_4_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_D1Chars_4_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_D1Chars_4_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_D1Chars_4_ipg_end);
    }
    nt_D1Chars_4_ipg_end += left;
    nt_D1Chars_4_ipg_start += left;
    left = nt_D1Chars_4_ipg_start;
    right = nt_D1Chars_4_ipg_end;

    // { bibliographicFileIdentifier = D1Chars@4.value }
    let mut self_bibliographicFileIdentifier = nt_D1Chars_4.value;

    // DateAndTime@0[D1Chars@4.END, D1Chars@4.END + 17]
    left = nt_D1Chars_4_ipg_end as usize;
    right = (nt_D1Chars_4_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_0_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_0_ipg_start, mut nt_DateAndTime_0_ipg_end, nt_DateAndTime_0) = match nt_DateAndTime_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_0_ipg_end);
    }
    nt_DateAndTime_0_ipg_end += left;
    nt_DateAndTime_0_ipg_start += left;
    left = nt_DateAndTime_0_ipg_start;
    right = nt_DateAndTime_0_ipg_end;

    // { volumeCreationDateAndTime = DateAndTime@0.this }
    let mut self_volumeCreationDateAndTime = nt_DateAndTime_0;

    // DateAndTime@1[DateAndTime@0.END, DateAndTime@0.END + 17]
    left = nt_DateAndTime_0_ipg_end as usize;
    right = (nt_DateAndTime_0_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_1_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_1_ipg_start, mut nt_DateAndTime_1_ipg_end, nt_DateAndTime_1) = match nt_DateAndTime_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_1_ipg_end);
    }
    nt_DateAndTime_1_ipg_end += left;
    nt_DateAndTime_1_ipg_start += left;
    left = nt_DateAndTime_1_ipg_start;
    right = nt_DateAndTime_1_ipg_end;

    // { volumeModificationDateAndTime = DateAndTime@1.this }
    let mut self_volumeModificationDateAndTime = nt_DateAndTime_1;

    // DateAndTime@2[DateAndTime@1.END, DateAndTime@1.END + 17]
    left = nt_DateAndTime_1_ipg_end as usize;
    right = (nt_DateAndTime_1_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_2_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_2_ipg_start, mut nt_DateAndTime_2_ipg_end, nt_DateAndTime_2) = match nt_DateAndTime_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_2_ipg_end);
    }
    nt_DateAndTime_2_ipg_end += left;
    nt_DateAndTime_2_ipg_start += left;
    left = nt_DateAndTime_2_ipg_start;
    right = nt_DateAndTime_2_ipg_end;

    // { volumeExpirationDateAndTime = DateAndTime@2.this }
    let mut self_volumeExpirationDateAndTime = nt_DateAndTime_2;

    // DateAndTime@3[DateAndTime@2.END, DateAndTime@2.END + 17]
    left = nt_DateAndTime_2_ipg_end as usize;
    right = (nt_DateAndTime_2_ipg_end + 17 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_3_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_3_ipg_start, mut nt_DateAndTime_3_ipg_end, nt_DateAndTime_3) = match nt_DateAndTime_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_3_ipg_end);
    }
    nt_DateAndTime_3_ipg_end += left;
    nt_DateAndTime_3_ipg_start += left;
    left = nt_DateAndTime_3_ipg_start;
    right = nt_DateAndTime_3_ipg_end;

    // { volumeEffectiveDateAndTime = DateAndTime@3.this }
    let mut self_volumeEffectiveDateAndTime = nt_DateAndTime_3;

    // { fileStructureVersion = .[DateAndTime@3.END] }
    left = nt_DateAndTime_3_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_fileStructureVersion = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // "\x00"[DateAndTime@3.END + 1, DateAndTime@3.END + 2]
    left = (nt_DateAndTime_3_ipg_end + 1 as usize) as usize;
    right = (nt_DateAndTime_3_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // HexBytes@1[DateAndTime@3.END + 2, DateAndTime@3.END + 514]
    left = (nt_DateAndTime_3_ipg_end + 2 as usize) as usize;
    right = (nt_DateAndTime_3_ipg_end + 514 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_1_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_1_ipg_start, mut nt_HexBytes_1_ipg_end, nt_HexBytes_1) = match nt_HexBytes_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_1_ipg_end);
    }
    nt_HexBytes_1_ipg_end += left;
    nt_HexBytes_1_ipg_start += left;
    left = nt_HexBytes_1_ipg_start;
    right = nt_HexBytes_1_ipg_end;

    // { applicationUse = HexBytes@1.value }
    let mut self_applicationUse = nt_HexBytes_1.value;

    // NULBytes@1[HexBytes@1.END, EOI]
    left = nt_HexBytes_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_1_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_1_ipg_start, mut nt_NULBytes_1_ipg_end, nt_NULBytes_1) = match nt_NULBytes_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_1_ipg_end);
    }
    nt_NULBytes_1_ipg_end += left;
    nt_NULBytes_1_ipg_start += left;
    left = nt_NULBytes_1_ipg_start;
    right = nt_NULBytes_1_ipg_end;

    // { descriptor = makeSupplementary(volumeDescriptorVersion, volumeFlags, systemIdentifier, volumeIdentifier, volumeSpaceSize, escapeSequences, volumeSetSize, volumeSequenceNumber, logicalBlockSize, pathTableSize, locationOfTypeLPathTable, locationOfOptionalTypeLPathTable, locationOfTypeMPathTable, locationOfOptionalTypeMPathTable, rootDirectoryRecord, volumeSetIdentifier, publisherIdentifier, dataPreparerIdentifier, applicationIdentifier, copyrightFileIdentifier, abstractFileIdentifier, bibliographicFileIdentifier, volumeCreationDateAndTime, volumeModificationDateAndTime, volumeExpirationDateAndTime, volumeEffectiveDateAndTime, fileStructureVersion, applicationUse) }
    let mut self_descriptor = makeSupplementary(self_volumeDescriptorVersion, self_volumeFlags, self_systemIdentifier, self_volumeIdentifier, self_volumeSpaceSize, self_escapeSequences, self_volumeSetSize, self_volumeSequenceNumber, self_logicalBlockSize, self_pathTableSize, self_locationOfTypeLPathTable, self_locationOfOptionalTypeLPathTable, self_locationOfTypeMPathTable, self_locationOfOptionalTypeMPathTable, self_rootDirectoryRecord, self_volumeSetIdentifier, self_publisherIdentifier, self_dataPreparerIdentifier, self_applicationIdentifier, self_copyrightFileIdentifier, self_abstractFileIdentifier, self_bibliographicFileIdentifier, self_volumeCreationDateAndTime, self_volumeModificationDateAndTime, self_volumeExpirationDateAndTime, self_volumeEffectiveDateAndTime, self_fileStructureVersion, self_applicationUse);

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn VolumePartitionDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x03CD001\x01\x00"[0, 8]
    left = 0 as usize;
    right = 8 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[3, 67, 68, 48, 48, 49, 1, 0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 8;
    self_ipg_end = self_ipg_end.max(right);

    // AChars@0[8, 40]
    left = 8 as usize;
    right = 40 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_0_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_0_ipg_start, mut nt_AChars_0_ipg_end, nt_AChars_0) = match nt_AChars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_0_ipg_end);
    }
    nt_AChars_0_ipg_end += left;
    nt_AChars_0_ipg_start += left;
    left = nt_AChars_0_ipg_start;
    right = nt_AChars_0_ipg_end;

    // { systemIdentifier = AChars@0.value }
    let mut self_systemIdentifier = nt_AChars_0.value;

    // DChars@0[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0_ipg_end as usize;
    right = (nt_AChars_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DChars_0_m = DChars(input, begin + left, begin + right);
    let (mut nt_DChars_0_ipg_start, mut nt_DChars_0_ipg_end, nt_DChars_0) = match nt_DChars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DChars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DChars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DChars_0_ipg_end);
    }
    nt_DChars_0_ipg_end += left;
    nt_DChars_0_ipg_start += left;
    left = nt_DChars_0_ipg_start;
    right = nt_DChars_0_ipg_end;

    // { volumePartitionIdentifier = DChars@0.value }
    let mut self_volumePartitionIdentifier = nt_DChars_0.value;

    // BB_U32@0[DChars@0.END, EOI]
    left = nt_DChars_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_0_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_0_ipg_start, mut nt_BB_U32_0_ipg_end, nt_BB_U32_0) = match nt_BB_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_0_ipg_end);
    }
    nt_BB_U32_0_ipg_end += left;
    nt_BB_U32_0_ipg_start += left;
    left = nt_BB_U32_0_ipg_start;
    right = nt_BB_U32_0_ipg_end;

    // { volumePartitionLocation = BB_U32@0.value }
    let mut self_volumePartitionLocation = nt_BB_U32_0.value;

    // BB_U32@1[BB_U32@0.END, EOI]
    left = nt_BB_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_1_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_1_ipg_start, mut nt_BB_U32_1_ipg_end, nt_BB_U32_1) = match nt_BB_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_1_ipg_end);
    }
    nt_BB_U32_1_ipg_end += left;
    nt_BB_U32_1_ipg_start += left;
    left = nt_BB_U32_1_ipg_start;
    right = nt_BB_U32_1_ipg_end;

    // { volumePartitionSize = BB_U32@1.value }
    let mut self_volumePartitionSize = nt_BB_U32_1.value;

    // HexBytes@0[BB_U32@1.END, EOI]
    left = nt_BB_U32_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { systemUse = HexBytes@0.value }
    let mut self_systemUse = nt_HexBytes_0.value;

    // { descriptor = makeVolumePartition(systemIdentifier, volumePartitionIdentifier, volumePartitionLocation, volumePartitionSize, systemUse) }
    let mut self_descriptor = makeVolumePartition(self_systemIdentifier, self_volumePartitionIdentifier, self_volumePartitionLocation, self_volumePartitionSize, self_systemUse);

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn BootRecord(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\x00CD001\x01"[0, 7]
    left = 0 as usize;
    right = 7 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0, 67, 68, 48, 48, 49, 1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 7;
    self_ipg_end = self_ipg_end.max(right);

    // AChars@0[7, 39]
    left = 7 as usize;
    right = 39 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_0_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_0_ipg_start, mut nt_AChars_0_ipg_end, nt_AChars_0) = match nt_AChars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_0_ipg_end);
    }
    nt_AChars_0_ipg_end += left;
    nt_AChars_0_ipg_start += left;
    left = nt_AChars_0_ipg_start;
    right = nt_AChars_0_ipg_end;

    // { bootSystemIdentifier = AChars@0.value }
    let mut self_bootSystemIdentifier = nt_AChars_0.value;

    // AChars@1[AChars@0.END, AChars@0.END + 32]
    left = nt_AChars_0_ipg_end as usize;
    right = (nt_AChars_0_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AChars_1_m = AChars(input, begin + left, begin + right);
    let (mut nt_AChars_1_ipg_start, mut nt_AChars_1_ipg_end, nt_AChars_1) = match nt_AChars_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AChars_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AChars_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AChars_1_ipg_end);
    }
    nt_AChars_1_ipg_end += left;
    nt_AChars_1_ipg_start += left;
    left = nt_AChars_1_ipg_start;
    right = nt_AChars_1_ipg_end;

    // { bootIdentifier = AChars@1.value }
    let mut self_bootIdentifier = nt_AChars_1.value;

    // HexBytes@0[AChars@1.END, EOI]
    left = nt_AChars_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { systemUse = HexBytes@0.value }
    let mut self_systemUse = nt_HexBytes_0.value;

    // { descriptor = makeBoot(bootSystemIdentifier, bootIdentifier, systemUse) }
    let mut self_descriptor = makeBoot(self_bootSystemIdentifier, self_bootIdentifier, self_systemUse);

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn VolumeDescriptorSetTerminator(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // "\xffCD001\x01"[0, 7]
    left = 0 as usize;
    right = 7 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[255, 67, 68, 48, 48, 49, 1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 7;
    self_ipg_end = self_ipg_end.max(right);

    // NULBytes@0[7, EOI]
    left = 7 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_0_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_0_ipg_start, mut nt_NULBytes_0_ipg_end, nt_NULBytes_0) = match nt_NULBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_0_ipg_end);
    }
    nt_NULBytes_0_ipg_end += left;
    nt_NULBytes_0_ipg_start += left;
    left = nt_NULBytes_0_ipg_start;
    right = nt_NULBytes_0_ipg_end;

    // { descriptor = Terminator() }
    let mut self_descriptor = Terminator();

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn UnknownVolumeDescriptor(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, VolumeDescriptor)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { typeByte = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_typeByte = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // "CD001"[1, 6]
    left = 1 as usize;
    right = 6 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[67, 68, 48, 48, 49]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 5;
    self_ipg_end = self_ipg_end.max(right);

    // { version = .[6] }
    left = 6 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_version = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { data = *[7, EOI] }
    left = 7 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_data = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { descriptor = Unknown(data) }
    let mut self_descriptor = Unknown(self_data);

    return Some((self_ipg_start, self_ipg_end, VolumeDescriptor {
      descriptor: self_descriptor,
    }));
  }

  return None;
}

fn DirectoriesRecursive(input: &[u8], begin: usize, end: usize, a_logicalBlockSize: i64, a_node: DirectoryRecord) -> Option<(usize, usize, DirectoriesRecursive)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ isDirectory(ref(node)) ]
    if !isDirectory(&(a_node)) { break '_ipg_alt; }

    // { offset = logicalBlockSize * locationOfExtent(ref(node)) }
    let mut self_offset = a_logicalBlockSize * locationOfExtent(&(a_node));

    // DirectoryRecords@0(false)[offset, offset + dataLength(ref(node))]
    left = self_offset as usize;
    right = (self_offset + dataLength(&(a_node))) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DirectoryRecords_0_m = DirectoryRecords(input, begin + left, begin + right, false);
    let (mut nt_DirectoryRecords_0_ipg_start, mut nt_DirectoryRecords_0_ipg_end, nt_DirectoryRecords_0) = match nt_DirectoryRecords_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DirectoryRecords_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DirectoryRecords_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DirectoryRecords_0_ipg_end);
    }
    nt_DirectoryRecords_0_ipg_end += left;
    nt_DirectoryRecords_0_ipg_start += left;
    left = nt_DirectoryRecords_0_ipg_start;
    right = nt_DirectoryRecords_0_ipg_end;

    // for i = 2 to length(ref(DirectoryRecords@0.values)) do DirectoriesRecursive@0(logicalBlockSize, clone(ref(DirectoryRecords@0.values[i])))[0, EOI]
    let mut nt_DirectoriesRecursive_0_ipg_start = left;
    let mut nt_DirectoriesRecursive_0_ipg_end = right;
    let seq_DirectoriesRecursive_0_start = 2 as usize;
    let loopEnd = length(&(nt_DirectoryRecords_0.values)) as usize;
    let mut seq_DirectoriesRecursive_0 = Vec::with_capacity(loopEnd.saturating_sub(seq_DirectoriesRecursive_0_start));
    for i_i in seq_DirectoriesRecursive_0_start..loopEnd {
      let left = 0 as usize;
      let right = EOI as usize;
      if right < left || right > EOI { break '_ipg_alt; }
      let tmp_m = DirectoriesRecursive(input, begin + left, begin + right, a_logicalBlockSize, clone(&(nt_DirectoryRecords_0.values[i_i])));
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
      nt_DirectoriesRecursive_0_ipg_end = tmp_ipg_end;
      nt_DirectoriesRecursive_0_ipg_start = tmp_ipg_start;
      seq_DirectoriesRecursive_0.push(tmp);
    }
    left = nt_DirectoriesRecursive_0_ipg_start;
    right = nt_DirectoriesRecursive_0_ipg_end;

    // { children = projectRoot(DirectoriesRecursive@0.these) }
    let mut self_children = projectRoot(seq_DirectoriesRecursive_0);

    // { root = DirectoryBranch(offset, node, children) }
    let mut self_root = DirectoryBranch(self_offset, a_node, self_children);

    return Some((self_ipg_start, self_ipg_end, DirectoriesRecursive {
      root: self_root,
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { offset = logicalBlockSize * locationOfExtent(ref(node)) }
    let mut self_offset = a_logicalBlockSize * locationOfExtent(&(a_node));

    // { root = FileLeaf(offset, node) }
    let mut self_root = FileLeaf(self_offset, a_node);

    return Some((self_ipg_start, self_ipg_end, DirectoriesRecursive {
      root: self_root,
    }));
  }

  return None;
}

fn DirectoryRecords(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, DirectoryRecords)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat DirectoryRecord@0(enhanced)[DirectoryRecord@0.END, EOI].this starting on [0, EOI]
    let mut self_values = Vec::new();
    left = 0 as usize;
    right = EOI as usize;
    let nt_DirectoryRecord_0_m = DirectoryRecord(input, begin + left, begin + right, a_enhanced);
    let mut nt_DirectoryRecord_0_ipg_start = right;
    let mut nt_DirectoryRecord_0_ipg_end = left;
    match nt_DirectoryRecord_0_m {
      None => {}
      Some((nt_DirectoryRecord_0_ipg_start_, nt_DirectoryRecord_0_ipg_end_, nt_DirectoryRecord_0)) => {
        nt_DirectoryRecord_0_ipg_start = nt_DirectoryRecord_0_ipg_start_;
        nt_DirectoryRecord_0_ipg_end = nt_DirectoryRecord_0_ipg_end_;
        if nt_DirectoryRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: DirectoryRecord"); }
        self_ipg_start = self_ipg_start.min(left + nt_DirectoryRecord_0_ipg_start);
        self_ipg_end = self_ipg_end.max(left + nt_DirectoryRecord_0_ipg_end);
        nt_DirectoryRecord_0_ipg_end += left;
        nt_DirectoryRecord_0_ipg_start += left;
        left = nt_DirectoryRecord_0_ipg_end as usize;
        right = EOI as usize;
        self_values.push(nt_DirectoryRecord_0);

        while left <= right && right <= EOI {
          let nt_DirectoryRecord_0_m = DirectoryRecord(input, begin + left, begin + right, a_enhanced);
          let (nt_DirectoryRecord_0_ipg_start_, nt_DirectoryRecord_0_ipg_end_, nt_DirectoryRecord_0) = match nt_DirectoryRecord_0_m {
            None => { break; }
            Some(p) => p,
          };
          nt_DirectoryRecord_0_ipg_start = nt_DirectoryRecord_0_ipg_start_;
          nt_DirectoryRecord_0_ipg_end = nt_DirectoryRecord_0_ipg_end_;
          if nt_DirectoryRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: DirectoryRecord"); }
          self_ipg_start = self_ipg_start.min(left + nt_DirectoryRecord_0_ipg_start);
          self_ipg_end = self_ipg_end.max(left + nt_DirectoryRecord_0_ipg_end);
          nt_DirectoryRecord_0_ipg_end += left;
          nt_DirectoryRecord_0_ipg_start += left;
          self_values.push(nt_DirectoryRecord_0);
          left = nt_DirectoryRecord_0_ipg_end as usize;
          right = EOI as usize;
        }
      }
    };

    return Some((self_ipg_start, self_ipg_end, DirectoryRecords {
      values: self_values,
    }));
  }

  return None;
}

fn DirectoryRecord(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, DirectoryRecord)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { length = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_length = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // ?[ length > 0 ]
    if !(self_length > 0) { break '_ipg_alt; }

    // { extendedAttributeRecordLength = .[1] }
    left = 1 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_extendedAttributeRecordLength = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // BB_U32@0[2, EOI]
    left = 2 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_0_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_0_ipg_start, mut nt_BB_U32_0_ipg_end, nt_BB_U32_0) = match nt_BB_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_0_ipg_end);
    }
    nt_BB_U32_0_ipg_end += left;
    nt_BB_U32_0_ipg_start += left;
    left = nt_BB_U32_0_ipg_start;
    right = nt_BB_U32_0_ipg_end;

    // { locationOfExtent = BB_U32@0.value }
    let mut self_locationOfExtent = nt_BB_U32_0.value;

    // BB_U32@1[BB_U32@0.END, EOI]
    left = nt_BB_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U32_1_m = BB_U32(input, begin + left, begin + right);
    let (mut nt_BB_U32_1_ipg_start, mut nt_BB_U32_1_ipg_end, nt_BB_U32_1) = match nt_BB_U32_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U32_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U32_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U32_1_ipg_end);
    }
    nt_BB_U32_1_ipg_end += left;
    nt_BB_U32_1_ipg_start += left;
    left = nt_BB_U32_1_ipg_start;
    right = nt_BB_U32_1_ipg_end;

    // { dataLength = BB_U32@1.value }
    let mut self_dataLength = nt_BB_U32_1.value;

    // RecordingDateAndTime@0[BB_U32@1.END, BB_U32@1.END + 7]
    left = nt_BB_U32_1_ipg_end as usize;
    right = (nt_BB_U32_1_ipg_end + 7 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_RecordingDateAndTime_0_m = RecordingDateAndTime(input, begin + left, begin + right);
    let (mut nt_RecordingDateAndTime_0_ipg_start, mut nt_RecordingDateAndTime_0_ipg_end, nt_RecordingDateAndTime_0) = match nt_RecordingDateAndTime_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_RecordingDateAndTime_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_RecordingDateAndTime_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_RecordingDateAndTime_0_ipg_end);
    }
    nt_RecordingDateAndTime_0_ipg_end += left;
    nt_RecordingDateAndTime_0_ipg_start += left;
    left = nt_RecordingDateAndTime_0_ipg_start;
    right = nt_RecordingDateAndTime_0_ipg_end;

    // { recordingDateAndTime = RecordingDateAndTime@0.this }
    let mut self_recordingDateAndTime = nt_RecordingDateAndTime_0;

    // Byte@0[RecordingDateAndTime@0.END, EOI]
    left = nt_RecordingDateAndTime_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Byte_0_m = Byte(input, begin + left, begin + right);
    let (mut nt_Byte_0_ipg_start, mut nt_Byte_0_ipg_end, nt_Byte_0) = match nt_Byte_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Byte_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Byte_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Byte_0_ipg_end);
    }
    nt_Byte_0_ipg_end += left;
    nt_Byte_0_ipg_start += left;
    left = nt_Byte_0_ipg_start;
    right = nt_Byte_0_ipg_end;

    // { isHidden = (1 & Byte@0.value) != 0 }
    let mut self_isHidden = (1 & nt_Byte_0.value) != 0;

    // { isDirectory = (2 & Byte@0.value) != 0 }
    let mut self_isDirectory = (2 & nt_Byte_0.value) != 0;

    // { isAssociatedFile = (4 & Byte@0.value) != 0 }
    let mut self_isAssociatedFile = (4 & nt_Byte_0.value) != 0;

    // { isRecord = (8 & Byte@0.value) != 0 }
    let mut self_isRecord = (8 & nt_Byte_0.value) != 0;

    // { hasPermissions = (16 & Byte@0.value) != 0 }
    let mut self_hasPermissions = (16 & nt_Byte_0.value) != 0;

    // { isMultiExtent = (128 & Byte@0.value) != 0 }
    let mut self_isMultiExtent = (128 & nt_Byte_0.value) != 0;

    // { fileUnitSize = .[Byte@0.END] }
    left = nt_Byte_0_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_fileUnitSize = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { interleaveGapSize = .[Byte@0.END + 1] }
    left = (nt_Byte_0_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_interleaveGapSize = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // BB_U16@0[Byte@0.END + 2, EOI]
    left = (nt_Byte_0_ipg_end + 2 as usize) as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_0_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_0_ipg_start, mut nt_BB_U16_0_ipg_end, nt_BB_U16_0) = match nt_BB_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_0_ipg_end);
    }
    nt_BB_U16_0_ipg_end += left;
    nt_BB_U16_0_ipg_start += left;
    left = nt_BB_U16_0_ipg_start;
    right = nt_BB_U16_0_ipg_end;

    // { volumeSequenceNumber = BB_U16@0.value }
    let mut self_volumeSequenceNumber = nt_BB_U16_0.value;

    // { lengthOfFileIdentifier = .[BB_U16@0.END] }
    left = nt_BB_U16_0_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_lengthOfFileIdentifier = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // DorD1Chars@0(enhanced)[BB_U16@0.END + 1, BB_U16@0.END + 1 + lengthOfFileIdentifier]
    left = (nt_BB_U16_0_ipg_end + 1 as usize) as usize;
    right = (nt_BB_U16_0_ipg_end + 1 + self_lengthOfFileIdentifier as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DorD1Chars_0_m = DorD1Chars(input, begin + left, begin + right, a_enhanced);
    let (mut nt_DorD1Chars_0_ipg_start, mut nt_DorD1Chars_0_ipg_end, nt_DorD1Chars_0) = match nt_DorD1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DorD1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DorD1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DorD1Chars_0_ipg_end);
    }
    nt_DorD1Chars_0_ipg_end += left;
    nt_DorD1Chars_0_ipg_start += left;
    left = nt_DorD1Chars_0_ipg_start;
    right = nt_DorD1Chars_0_ipg_end;

    // { fileIdentifier = DorD1Chars@0.value }
    let mut self_fileIdentifier = nt_DorD1Chars_0.value;

    // EvenPadByte@0(lengthOfFileIdentifier)[DorD1Chars@0.END, EOI]
    left = nt_DorD1Chars_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_EvenPadByte_0_m = EvenPadByte(input, begin + left, begin + right, self_lengthOfFileIdentifier);
    let (mut nt_EvenPadByte_0_ipg_start, mut nt_EvenPadByte_0_ipg_end, nt_EvenPadByte_0) = match nt_EvenPadByte_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_EvenPadByte_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_EvenPadByte_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_EvenPadByte_0_ipg_end);
    }
    nt_EvenPadByte_0_ipg_end += left;
    nt_EvenPadByte_0_ipg_start += left;
    left = nt_EvenPadByte_0_ipg_start;
    right = nt_EvenPadByte_0_ipg_end;

    // HexBytes@0[EvenPadByte@0.END, length]
    left = nt_EvenPadByte_0_ipg_end as usize;
    right = self_length as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { systemUse = HexBytes@0.value }
    let mut self_systemUse = nt_HexBytes_0.value;

    return Some((self_ipg_start, self_ipg_end, DirectoryRecord {
      dataLength: self_dataLength,
      extendedAttributeRecordLength: self_extendedAttributeRecordLength,
      fileIdentifier: self_fileIdentifier,
      fileUnitSize: self_fileUnitSize,
      hasPermissions: self_hasPermissions,
      interleaveGapSize: self_interleaveGapSize,
      isAssociatedFile: self_isAssociatedFile,
      isDirectory: self_isDirectory,
      isHidden: self_isHidden,
      isMultiExtent: self_isMultiExtent,
      isRecord: self_isRecord,
      length: self_length,
      lengthOfFileIdentifier: self_lengthOfFileIdentifier,
      locationOfExtent: self_locationOfExtent,
      recordingDateAndTime: self_recordingDateAndTime,
      systemUse: self_systemUse,
      volumeSequenceNumber: self_volumeSequenceNumber,
    }));
  }

  return None;
}

fn RecordingDateAndTime(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, RecordingDateAndTime)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // Byte@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Byte_0_m = Byte(input, begin + left, begin + right);
    let (mut nt_Byte_0_ipg_start, mut nt_Byte_0_ipg_end, nt_Byte_0) = match nt_Byte_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Byte_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Byte_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Byte_0_ipg_end);
    }
    nt_Byte_0_ipg_end += left;
    nt_Byte_0_ipg_start += left;
    left = nt_Byte_0_ipg_start;
    right = nt_Byte_0_ipg_end;

    // { year = (Byte@0.value :: Int) + 1900 }
    let mut self_year = (nt_Byte_0.value as i64) + 1900;

    // { month = .[Byte@0.END] }
    left = nt_Byte_0_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_month = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { day = .[Byte@0.END + 1] }
    left = (nt_Byte_0_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_day = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { hour = .[Byte@0.END + 2] }
    left = (nt_Byte_0_ipg_end + 2) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_hour = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { minute = .[Byte@0.END + 3] }
    left = (nt_Byte_0_ipg_end + 3) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_minute = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { second = .[Byte@0.END + 4] }
    left = (nt_Byte_0_ipg_end + 4) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_second = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { gmtOffset = .[Byte@0.END + 5] }
    left = (nt_Byte_0_ipg_end + 5) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_gmtOffset = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, RecordingDateAndTime {
      day: self_day,
      gmtOffset: self_gmtOffset,
      hour: self_hour,
      minute: self_minute,
      month: self_month,
      second: self_second,
      year: self_year,
    }));
  }

  return None;
}

fn LPathTableRecords(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, LPathTableRecords)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat LPathTableRecord@0(enhanced)[LPathTableRecord@0.END, EOI].this starting on [0, EOI]
    let mut self_values = Vec::new();
    left = 0 as usize;
    right = EOI as usize;
    let nt_LPathTableRecord_0_m = LPathTableRecord(input, begin + left, begin + right, a_enhanced);
    let mut nt_LPathTableRecord_0_ipg_start = right;
    let mut nt_LPathTableRecord_0_ipg_end = left;
    match nt_LPathTableRecord_0_m {
      None => {}
      Some((nt_LPathTableRecord_0_ipg_start_, nt_LPathTableRecord_0_ipg_end_, nt_LPathTableRecord_0)) => {
        nt_LPathTableRecord_0_ipg_start = nt_LPathTableRecord_0_ipg_start_;
        nt_LPathTableRecord_0_ipg_end = nt_LPathTableRecord_0_ipg_end_;
        if nt_LPathTableRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: LPathTableRecord"); }
        self_ipg_start = self_ipg_start.min(left + nt_LPathTableRecord_0_ipg_start);
        self_ipg_end = self_ipg_end.max(left + nt_LPathTableRecord_0_ipg_end);
        nt_LPathTableRecord_0_ipg_end += left;
        nt_LPathTableRecord_0_ipg_start += left;
        left = nt_LPathTableRecord_0_ipg_end as usize;
        right = EOI as usize;
        self_values.push(nt_LPathTableRecord_0);

        while left <= right && right <= EOI {
          let nt_LPathTableRecord_0_m = LPathTableRecord(input, begin + left, begin + right, a_enhanced);
          let (nt_LPathTableRecord_0_ipg_start_, nt_LPathTableRecord_0_ipg_end_, nt_LPathTableRecord_0) = match nt_LPathTableRecord_0_m {
            None => { break; }
            Some(p) => p,
          };
          nt_LPathTableRecord_0_ipg_start = nt_LPathTableRecord_0_ipg_start_;
          nt_LPathTableRecord_0_ipg_end = nt_LPathTableRecord_0_ipg_end_;
          if nt_LPathTableRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: LPathTableRecord"); }
          self_ipg_start = self_ipg_start.min(left + nt_LPathTableRecord_0_ipg_start);
          self_ipg_end = self_ipg_end.max(left + nt_LPathTableRecord_0_ipg_end);
          nt_LPathTableRecord_0_ipg_end += left;
          nt_LPathTableRecord_0_ipg_start += left;
          self_values.push(nt_LPathTableRecord_0);
          left = nt_LPathTableRecord_0_ipg_end as usize;
          right = EOI as usize;
        }
      }
    };

    return Some((self_ipg_start, self_ipg_end, LPathTableRecords {
      values: self_values,
    }));
  }

  return None;
}

fn LPathTableRecord(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, PathTableRecord)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { lengthOfDirectoryIdentifier = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_lengthOfDirectoryIdentifier = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { extendedAttributeRecordLength = .[1] }
    left = 1 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_extendedAttributeRecordLength = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // LE_U32@0[2, EOI]
    left = 2 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_0_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_0_ipg_start, mut nt_LE_U32_0_ipg_end, nt_LE_U32_0) = match nt_LE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_0_ipg_end);
    }
    nt_LE_U32_0_ipg_end += left;
    nt_LE_U32_0_ipg_start += left;
    left = nt_LE_U32_0_ipg_start;
    right = nt_LE_U32_0_ipg_end;

    // { locationOfExtent = LE_U32@0.value }
    let mut self_locationOfExtent = nt_LE_U32_0.value;

    // LE_U16@0[LE_U32@0.END, EOI]
    left = nt_LE_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U16_0_m = LE_U16(input, begin + left, begin + right);
    let (mut nt_LE_U16_0_ipg_start, mut nt_LE_U16_0_ipg_end, nt_LE_U16_0) = match nt_LE_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U16_0_ipg_end);
    }
    nt_LE_U16_0_ipg_end += left;
    nt_LE_U16_0_ipg_start += left;
    left = nt_LE_U16_0_ipg_start;
    right = nt_LE_U16_0_ipg_end;

    // { parentDirectoryNumber = LE_U16@0.value }
    let mut self_parentDirectoryNumber = nt_LE_U16_0.value;

    // DorD1Chars@0(enhanced)[LE_U16@0.END, LE_U16@0.END + lengthOfDirectoryIdentifier]
    left = nt_LE_U16_0_ipg_end as usize;
    right = (nt_LE_U16_0_ipg_end + self_lengthOfDirectoryIdentifier as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DorD1Chars_0_m = DorD1Chars(input, begin + left, begin + right, a_enhanced);
    let (mut nt_DorD1Chars_0_ipg_start, mut nt_DorD1Chars_0_ipg_end, nt_DorD1Chars_0) = match nt_DorD1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DorD1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DorD1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DorD1Chars_0_ipg_end);
    }
    nt_DorD1Chars_0_ipg_end += left;
    nt_DorD1Chars_0_ipg_start += left;
    left = nt_DorD1Chars_0_ipg_start;
    right = nt_DorD1Chars_0_ipg_end;

    // { directoryIdentifier = DorD1Chars@0.value }
    let mut self_directoryIdentifier = nt_DorD1Chars_0.value;

    // OddPadByte@0(lengthOfDirectoryIdentifier)[DorD1Chars@0.END, EOI]
    left = nt_DorD1Chars_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_OddPadByte_0_m = OddPadByte(input, begin + left, begin + right, self_lengthOfDirectoryIdentifier);
    let (mut nt_OddPadByte_0_ipg_start, mut nt_OddPadByte_0_ipg_end, nt_OddPadByte_0) = match nt_OddPadByte_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_OddPadByte_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_OddPadByte_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_OddPadByte_0_ipg_end);
    }
    nt_OddPadByte_0_ipg_end += left;
    nt_OddPadByte_0_ipg_start += left;
    left = nt_OddPadByte_0_ipg_start;
    right = nt_OddPadByte_0_ipg_end;

    return Some((self_ipg_start, self_ipg_end, PathTableRecord {
      directoryIdentifier: self_directoryIdentifier,
      extendedAttributeRecordLength: self_extendedAttributeRecordLength,
      lengthOfDirectoryIdentifier: self_lengthOfDirectoryIdentifier,
      locationOfExtent: self_locationOfExtent,
      parentDirectoryNumber: self_parentDirectoryNumber,
    }));
  }

  return None;
}

fn MPathTableRecords(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, MPathTableRecords)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat MPathTableRecord@0(enhanced)[MPathTableRecord@0.END, EOI].this starting on [0, EOI]
    let mut self_values = Vec::new();
    left = 0 as usize;
    right = EOI as usize;
    let nt_MPathTableRecord_0_m = MPathTableRecord(input, begin + left, begin + right, a_enhanced);
    let mut nt_MPathTableRecord_0_ipg_start = right;
    let mut nt_MPathTableRecord_0_ipg_end = left;
    match nt_MPathTableRecord_0_m {
      None => {}
      Some((nt_MPathTableRecord_0_ipg_start_, nt_MPathTableRecord_0_ipg_end_, nt_MPathTableRecord_0)) => {
        nt_MPathTableRecord_0_ipg_start = nt_MPathTableRecord_0_ipg_start_;
        nt_MPathTableRecord_0_ipg_end = nt_MPathTableRecord_0_ipg_end_;
        if nt_MPathTableRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: MPathTableRecord"); }
        self_ipg_start = self_ipg_start.min(left + nt_MPathTableRecord_0_ipg_start);
        self_ipg_end = self_ipg_end.max(left + nt_MPathTableRecord_0_ipg_end);
        nt_MPathTableRecord_0_ipg_end += left;
        nt_MPathTableRecord_0_ipg_start += left;
        left = nt_MPathTableRecord_0_ipg_end as usize;
        right = EOI as usize;
        self_values.push(nt_MPathTableRecord_0);

        while left <= right && right <= EOI {
          let nt_MPathTableRecord_0_m = MPathTableRecord(input, begin + left, begin + right, a_enhanced);
          let (nt_MPathTableRecord_0_ipg_start_, nt_MPathTableRecord_0_ipg_end_, nt_MPathTableRecord_0) = match nt_MPathTableRecord_0_m {
            None => { break; }
            Some(p) => p,
          };
          nt_MPathTableRecord_0_ipg_start = nt_MPathTableRecord_0_ipg_start_;
          nt_MPathTableRecord_0_ipg_end = nt_MPathTableRecord_0_ipg_end_;
          if nt_MPathTableRecord_0_ipg_end == 0 { panic!("repeat of non-consuming rule: MPathTableRecord"); }
          self_ipg_start = self_ipg_start.min(left + nt_MPathTableRecord_0_ipg_start);
          self_ipg_end = self_ipg_end.max(left + nt_MPathTableRecord_0_ipg_end);
          nt_MPathTableRecord_0_ipg_end += left;
          nt_MPathTableRecord_0_ipg_start += left;
          self_values.push(nt_MPathTableRecord_0);
          left = nt_MPathTableRecord_0_ipg_end as usize;
          right = EOI as usize;
        }
      }
    };

    return Some((self_ipg_start, self_ipg_end, MPathTableRecords {
      values: self_values,
    }));
  }

  return None;
}

fn MPathTableRecord(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, PathTableRecord)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { lengthOfDirectoryIdentifier = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_lengthOfDirectoryIdentifier = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { extendedAttributeRecordLength = .[1] }
    left = 1 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_extendedAttributeRecordLength = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // BE_U32@0[2, EOI]
    left = 2 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_0_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_0_ipg_start, mut nt_BE_U32_0_ipg_end, nt_BE_U32_0) = match nt_BE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_0_ipg_end);
    }
    nt_BE_U32_0_ipg_end += left;
    nt_BE_U32_0_ipg_start += left;
    left = nt_BE_U32_0_ipg_start;
    right = nt_BE_U32_0_ipg_end;

    // { locationOfExtent = BE_U32@0.value }
    let mut self_locationOfExtent = nt_BE_U32_0.value;

    // BE_U16@0[BE_U32@0.END, EOI]
    left = nt_BE_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U16_0_m = BE_U16(input, begin + left, begin + right);
    let (mut nt_BE_U16_0_ipg_start, mut nt_BE_U16_0_ipg_end, nt_BE_U16_0) = match nt_BE_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U16_0_ipg_end);
    }
    nt_BE_U16_0_ipg_end += left;
    nt_BE_U16_0_ipg_start += left;
    left = nt_BE_U16_0_ipg_start;
    right = nt_BE_U16_0_ipg_end;

    // { parentDirectoryNumber = BE_U16@0.value }
    let mut self_parentDirectoryNumber = nt_BE_U16_0.value;

    // DorD1Chars@0(enhanced)[BE_U16@0.END, BE_U16@0.END + lengthOfDirectoryIdentifier]
    left = nt_BE_U16_0_ipg_end as usize;
    right = (nt_BE_U16_0_ipg_end + self_lengthOfDirectoryIdentifier as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DorD1Chars_0_m = DorD1Chars(input, begin + left, begin + right, a_enhanced);
    let (mut nt_DorD1Chars_0_ipg_start, mut nt_DorD1Chars_0_ipg_end, nt_DorD1Chars_0) = match nt_DorD1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DorD1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DorD1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DorD1Chars_0_ipg_end);
    }
    nt_DorD1Chars_0_ipg_end += left;
    nt_DorD1Chars_0_ipg_start += left;
    left = nt_DorD1Chars_0_ipg_start;
    right = nt_DorD1Chars_0_ipg_end;

    // { directoryIdentifier = DorD1Chars@0.value }
    let mut self_directoryIdentifier = nt_DorD1Chars_0.value;

    // OddPadByte@0(lengthOfDirectoryIdentifier)[DorD1Chars@0.END, EOI]
    left = nt_DorD1Chars_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_OddPadByte_0_m = OddPadByte(input, begin + left, begin + right, self_lengthOfDirectoryIdentifier);
    let (mut nt_OddPadByte_0_ipg_start, mut nt_OddPadByte_0_ipg_end, nt_OddPadByte_0) = match nt_OddPadByte_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_OddPadByte_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_OddPadByte_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_OddPadByte_0_ipg_end);
    }
    nt_OddPadByte_0_ipg_end += left;
    nt_OddPadByte_0_ipg_start += left;
    left = nt_OddPadByte_0_ipg_start;
    right = nt_OddPadByte_0_ipg_end;

    return Some((self_ipg_start, self_ipg_end, PathTableRecord {
      directoryIdentifier: self_directoryIdentifier,
      extendedAttributeRecordLength: self_extendedAttributeRecordLength,
      lengthOfDirectoryIdentifier: self_lengthOfDirectoryIdentifier,
      locationOfExtent: self_locationOfExtent,
      parentDirectoryNumber: self_parentDirectoryNumber,
    }));
  }

  return None;
}

fn ExtendedAttributeRecord(input: &[u8], begin: usize, end: usize, a_enhanced: bool) -> Option<(usize, usize, ExtendedAttributeRecord)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // BB_U16@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_0_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_0_ipg_start, mut nt_BB_U16_0_ipg_end, nt_BB_U16_0) = match nt_BB_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_0_ipg_end);
    }
    nt_BB_U16_0_ipg_end += left;
    nt_BB_U16_0_ipg_start += left;
    left = nt_BB_U16_0_ipg_start;
    right = nt_BB_U16_0_ipg_end;

    // { ownerIdentification = BB_U16@0.value }
    let mut self_ownerIdentification = nt_BB_U16_0.value;

    // BB_U16@1[BB_U16@0.END, EOI]
    left = nt_BB_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_1_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_1_ipg_start, mut nt_BB_U16_1_ipg_end, nt_BB_U16_1) = match nt_BB_U16_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_1_ipg_end);
    }
    nt_BB_U16_1_ipg_end += left;
    nt_BB_U16_1_ipg_start += left;
    left = nt_BB_U16_1_ipg_start;
    right = nt_BB_U16_1_ipg_end;

    // { groupIdentification = BB_U16@1.value }
    let mut self_groupIdentification = nt_BB_U16_1.value;

    // BE_U16@0[BB_U16@1.END, EOI]
    left = nt_BB_U16_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U16_0_m = BE_U16(input, begin + left, begin + right);
    let (mut nt_BE_U16_0_ipg_start, mut nt_BE_U16_0_ipg_end, nt_BE_U16_0) = match nt_BE_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U16_0_ipg_end);
    }
    nt_BE_U16_0_ipg_end += left;
    nt_BE_U16_0_ipg_start += left;
    left = nt_BE_U16_0_ipg_start;
    right = nt_BE_U16_0_ipg_end;

    // { permissions = BE_U16@0.value }
    let mut self_permissions = nt_BE_U16_0.value;

    // DateAndTime@0[BE_U16@0.END, EOI]
    left = nt_BE_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_0_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_0_ipg_start, mut nt_DateAndTime_0_ipg_end, nt_DateAndTime_0) = match nt_DateAndTime_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_0_ipg_end);
    }
    nt_DateAndTime_0_ipg_end += left;
    nt_DateAndTime_0_ipg_start += left;
    left = nt_DateAndTime_0_ipg_start;
    right = nt_DateAndTime_0_ipg_end;

    // { fileCreationDateAndTime = DateAndTime@0.this }
    let mut self_fileCreationDateAndTime = nt_DateAndTime_0;

    // DateAndTime@1[DateAndTime@0.END, EOI]
    left = nt_DateAndTime_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_1_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_1_ipg_start, mut nt_DateAndTime_1_ipg_end, nt_DateAndTime_1) = match nt_DateAndTime_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_1_ipg_end);
    }
    nt_DateAndTime_1_ipg_end += left;
    nt_DateAndTime_1_ipg_start += left;
    left = nt_DateAndTime_1_ipg_start;
    right = nt_DateAndTime_1_ipg_end;

    // { fileModificationDateAndTime = DateAndTime@1.this }
    let mut self_fileModificationDateAndTime = nt_DateAndTime_1;

    // DateAndTime@2[DateAndTime@1.END, EOI]
    left = nt_DateAndTime_1_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_2_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_2_ipg_start, mut nt_DateAndTime_2_ipg_end, nt_DateAndTime_2) = match nt_DateAndTime_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_2_ipg_end);
    }
    nt_DateAndTime_2_ipg_end += left;
    nt_DateAndTime_2_ipg_start += left;
    left = nt_DateAndTime_2_ipg_start;
    right = nt_DateAndTime_2_ipg_end;

    // { fileExpirationDateAndTime = DateAndTime@2.this }
    let mut self_fileExpirationDateAndTime = nt_DateAndTime_2;

    // DateAndTime@3[DateAndTime@2.END, EOI]
    left = nt_DateAndTime_2_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_DateAndTime_3_m = DateAndTime(input, begin + left, begin + right);
    let (mut nt_DateAndTime_3_ipg_start, mut nt_DateAndTime_3_ipg_end, nt_DateAndTime_3) = match nt_DateAndTime_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_DateAndTime_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_DateAndTime_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_DateAndTime_3_ipg_end);
    }
    nt_DateAndTime_3_ipg_end += left;
    nt_DateAndTime_3_ipg_start += left;
    left = nt_DateAndTime_3_ipg_start;
    right = nt_DateAndTime_3_ipg_end;

    // { fileEffectiveDateAndTime = DateAndTime@3.this }
    let mut self_fileEffectiveDateAndTime = nt_DateAndTime_3;

    // { recordFormat = .[DateAndTime@3.END] }
    left = nt_DateAndTime_3_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_recordFormat = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // { recordAttributes = .[DateAndTime@3.END + 1] }
    left = (nt_DateAndTime_3_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_recordAttributes = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // BB_U16@2[DateAndTime@3.END + 2, EOI]
    left = (nt_DateAndTime_3_ipg_end + 2 as usize) as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_2_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_2_ipg_start, mut nt_BB_U16_2_ipg_end, nt_BB_U16_2) = match nt_BB_U16_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_2_ipg_end);
    }
    nt_BB_U16_2_ipg_end += left;
    nt_BB_U16_2_ipg_start += left;
    left = nt_BB_U16_2_ipg_start;
    right = nt_BB_U16_2_ipg_end;

    // { recordLength = BB_U16@2.value }
    let mut self_recordLength = nt_BB_U16_2.value;

    // AorA1Chars@0(enhanced)[BB_U16@2.END, BB_U16@2.END + 32]
    left = nt_BB_U16_2_ipg_end as usize;
    right = (nt_BB_U16_2_ipg_end + 32 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_AorA1Chars_0_m = AorA1Chars(input, begin + left, begin + right, a_enhanced);
    let (mut nt_AorA1Chars_0_ipg_start, mut nt_AorA1Chars_0_ipg_end, nt_AorA1Chars_0) = match nt_AorA1Chars_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_AorA1Chars_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_AorA1Chars_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_AorA1Chars_0_ipg_end);
    }
    nt_AorA1Chars_0_ipg_end += left;
    nt_AorA1Chars_0_ipg_start += left;
    left = nt_AorA1Chars_0_ipg_start;
    right = nt_AorA1Chars_0_ipg_end;

    // { systemIdentifier = AorA1Chars@0.value }
    let mut self_systemIdentifier = nt_AorA1Chars_0.value;

    // HexBytes@0[AorA1Chars@0.END, AorA1Chars@0.END + 64]
    left = nt_AorA1Chars_0_ipg_end as usize;
    right = (nt_AorA1Chars_0_ipg_end + 64 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_0_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_0_ipg_start, mut nt_HexBytes_0_ipg_end, nt_HexBytes_0) = match nt_HexBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_0_ipg_end);
    }
    nt_HexBytes_0_ipg_end += left;
    nt_HexBytes_0_ipg_start += left;
    left = nt_HexBytes_0_ipg_start;
    right = nt_HexBytes_0_ipg_end;

    // { systemUse = HexBytes@0.value }
    let mut self_systemUse = nt_HexBytes_0.value;

    // "\x01"[HexBytes@0.END, HexBytes@0.END + 1]
    left = nt_HexBytes_0_ipg_end as usize;
    right = (nt_HexBytes_0_ipg_end + 1 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[1]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    // { lengthOfEscapeSequences = .[HexBytes@0.END + 1] }
    left = (nt_HexBytes_0_ipg_end + 1) as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_lengthOfEscapeSequences = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // NULBytes@0[HexBytes@0.END + 2, HexBytes@0.END + 66]
    left = (nt_HexBytes_0_ipg_end + 2 as usize) as usize;
    right = (nt_HexBytes_0_ipg_end + 66 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_NULBytes_0_m = NULBytes(input, begin + left, begin + right);
    let (mut nt_NULBytes_0_ipg_start, mut nt_NULBytes_0_ipg_end, nt_NULBytes_0) = match nt_NULBytes_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_NULBytes_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_NULBytes_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_NULBytes_0_ipg_end);
    }
    nt_NULBytes_0_ipg_end += left;
    nt_NULBytes_0_ipg_start += left;
    left = nt_NULBytes_0_ipg_start;
    right = nt_NULBytes_0_ipg_end;

    // BB_U16@3[NULBytes@0.END, EOI]
    left = nt_NULBytes_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BB_U16_3_m = BB_U16(input, begin + left, begin + right);
    let (mut nt_BB_U16_3_ipg_start, mut nt_BB_U16_3_ipg_end, nt_BB_U16_3) = match nt_BB_U16_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BB_U16_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BB_U16_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BB_U16_3_ipg_end);
    }
    nt_BB_U16_3_ipg_end += left;
    nt_BB_U16_3_ipg_start += left;
    left = nt_BB_U16_3_ipg_start;
    right = nt_BB_U16_3_ipg_end;

    // { lengthOfApplicationUse = BB_U16@3.value }
    let mut self_lengthOfApplicationUse = nt_BB_U16_3.value;

    // HexBytes@1[BB_U16@3.END, BB_U16@3.END + lengthOfApplicationUse]
    left = nt_BB_U16_3_ipg_end as usize;
    right = (nt_BB_U16_3_ipg_end + self_lengthOfApplicationUse as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_1_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_1_ipg_start, mut nt_HexBytes_1_ipg_end, nt_HexBytes_1) = match nt_HexBytes_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_1_ipg_end);
    }
    nt_HexBytes_1_ipg_end += left;
    nt_HexBytes_1_ipg_start += left;
    left = nt_HexBytes_1_ipg_start;
    right = nt_HexBytes_1_ipg_end;

    // { applicationUse = HexBytes@1.value }
    let mut self_applicationUse = nt_HexBytes_1.value;

    // HexBytes@2[HexBytes@1.END, HexBytes@1.END + lengthOfEscapeSequences]
    left = nt_HexBytes_1_ipg_end as usize;
    right = (nt_HexBytes_1_ipg_end + self_lengthOfEscapeSequences as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_HexBytes_2_m = HexBytes(input, begin + left, begin + right);
    let (mut nt_HexBytes_2_ipg_start, mut nt_HexBytes_2_ipg_end, nt_HexBytes_2) = match nt_HexBytes_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_HexBytes_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_HexBytes_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_HexBytes_2_ipg_end);
    }
    nt_HexBytes_2_ipg_end += left;
    nt_HexBytes_2_ipg_start += left;
    left = nt_HexBytes_2_ipg_start;
    right = nt_HexBytes_2_ipg_end;

    // { escapeSequences = HexBytes@2.value }
    let mut self_escapeSequences = nt_HexBytes_2.value;

    return Some((self_ipg_start, self_ipg_end, ExtendedAttributeRecord {
      applicationUse: self_applicationUse,
      escapeSequences: self_escapeSequences,
      fileCreationDateAndTime: self_fileCreationDateAndTime,
      fileEffectiveDateAndTime: self_fileEffectiveDateAndTime,
      fileExpirationDateAndTime: self_fileExpirationDateAndTime,
      fileModificationDateAndTime: self_fileModificationDateAndTime,
      groupIdentification: self_groupIdentification,
      lengthOfApplicationUse: self_lengthOfApplicationUse,
      lengthOfEscapeSequences: self_lengthOfEscapeSequences,
      ownerIdentification: self_ownerIdentification,
      permissions: self_permissions,
      recordAttributes: self_recordAttributes,
      recordFormat: self_recordFormat,
      recordLength: self_recordLength,
      systemIdentifier: self_systemIdentifier,
      systemUse: self_systemUse,
    }));
  }

  return None;
}

fn EvenPadByte(input: &[u8], begin: usize, end: usize, a_n: u8) -> Option<(usize, usize, EvenPadByte)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ n % 2 == 0 ]
    if !(a_n % 2 == 0) { break '_ipg_alt; }

    // "\x00"[0, 1]
    left = 0 as usize;
    right = 1 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, EvenPadByte {
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ""[0, 0]
    left = 0 as usize;
    right = 0 as usize;
    if right < left || right > EOI { break '_ipg_alt; }

    return Some((self_ipg_start, self_ipg_end, EvenPadByte {
    }));
  }

  return None;
}

fn OddPadByte(input: &[u8], begin: usize, end: usize, a_n: u8) -> Option<(usize, usize, OddPadByte)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ?[ n % 2 == 1 ]
    if !(a_n % 2 == 1) { break '_ipg_alt; }

    // "\x00"[0, 1]
    left = 0 as usize;
    right = 1 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    if !&input[begin + left .. begin + right].starts_with(&[0]) { break '_ipg_alt; }
    self_ipg_start = self_ipg_start.min(left);
    right = left + 1;
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, OddPadByte {
    }));
  }
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // ""[0, 0]
    left = 0 as usize;
    right = 0 as usize;
    if right < left || right > EOI { break '_ipg_alt; }

    return Some((self_ipg_start, self_ipg_end, OddPadByte {
    }));
  }

  return None;
}

fn A1Chars(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, A1Chars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = decodeUtf16(bytes) }
    let mut self_value = decodeUtf16(self_bytes);

    return Some((self_ipg_start, self_ipg_end, A1Chars {
      value: self_value,
    }));
  }

  return None;
}

fn D1Chars(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, D1Chars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = decodeUtf16(bytes) }
    let mut self_value = decodeUtf16(self_bytes);

    return Some((self_ipg_start, self_ipg_end, D1Chars {
      value: self_value,
    }));
  }

  return None;
}

fn DorD1Chars(input: &[u8], begin: usize, end: usize, a_isEnhanced: bool) -> Option<(usize, usize, DorD1Chars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = isEnhanced ? decodeUtf16(bytes) : decodeAscii(bytes) }
    let mut self_value = if a_isEnhanced { decodeUtf16(self_bytes) } else { decodeAscii(self_bytes) };

    return Some((self_ipg_start, self_ipg_end, DorD1Chars {
      value: self_value,
    }));
  }

  return None;
}

fn AorA1Chars(input: &[u8], begin: usize, end: usize, a_isEnhanced: bool) -> Option<(usize, usize, AorA1Chars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = isEnhanced ? decodeUtf16(bytes) : decodeAscii(bytes) }
    let mut self_value = if a_isEnhanced { decodeUtf16(self_bytes) } else { decodeAscii(self_bytes) };

    return Some((self_ipg_start, self_ipg_end, AorA1Chars {
      value: self_value,
    }));
  }

  return None;
}

fn AChars(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, AChars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = decodeAscii(bytes) }
    let mut self_value = decodeAscii(self_bytes);

    return Some((self_ipg_start, self_ipg_end, AChars {
      value: self_value,
    }));
  }

  return None;
}

fn DChars(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, DChars)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = decodeAscii(bytes) }
    let mut self_value = decodeAscii(self_bytes);

    return Some((self_ipg_start, self_ipg_end, DChars {
      value: self_value,
    }));
  }

  return None;
}

fn DateAndTime(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, DateAndTime)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // Digits@0[0, 4]
    left = 0 as usize;
    right = 4 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_0_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_0_ipg_start, mut nt_Digits_0_ipg_end, nt_Digits_0) = match nt_Digits_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_0_ipg_end);
    }
    nt_Digits_0_ipg_end += left;
    nt_Digits_0_ipg_start += left;
    left = nt_Digits_0_ipg_start;
    right = nt_Digits_0_ipg_end;

    // { year = Digits@0.value }
    let mut self_year = nt_Digits_0.value;

    // Digits@1[Digits@0.END, Digits@0.END + 2]
    left = nt_Digits_0_ipg_end as usize;
    right = (nt_Digits_0_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_1_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_1_ipg_start, mut nt_Digits_1_ipg_end, nt_Digits_1) = match nt_Digits_1_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_1_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_1_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_1_ipg_end);
    }
    nt_Digits_1_ipg_end += left;
    nt_Digits_1_ipg_start += left;
    left = nt_Digits_1_ipg_start;
    right = nt_Digits_1_ipg_end;

    // { month = Digits@1.value }
    let mut self_month = nt_Digits_1.value;

    // Digits@2[Digits@1.END, Digits@1.END + 2]
    left = nt_Digits_1_ipg_end as usize;
    right = (nt_Digits_1_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_2_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_2_ipg_start, mut nt_Digits_2_ipg_end, nt_Digits_2) = match nt_Digits_2_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_2_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_2_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_2_ipg_end);
    }
    nt_Digits_2_ipg_end += left;
    nt_Digits_2_ipg_start += left;
    left = nt_Digits_2_ipg_start;
    right = nt_Digits_2_ipg_end;

    // { day = Digits@2.value }
    let mut self_day = nt_Digits_2.value;

    // Digits@3[Digits@2.END, Digits@2.END + 2]
    left = nt_Digits_2_ipg_end as usize;
    right = (nt_Digits_2_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_3_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_3_ipg_start, mut nt_Digits_3_ipg_end, nt_Digits_3) = match nt_Digits_3_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_3_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_3_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_3_ipg_end);
    }
    nt_Digits_3_ipg_end += left;
    nt_Digits_3_ipg_start += left;
    left = nt_Digits_3_ipg_start;
    right = nt_Digits_3_ipg_end;

    // { hour = Digits@3.value }
    let mut self_hour = nt_Digits_3.value;

    // Digits@4[Digits@3.END, Digits@3.END + 2]
    left = nt_Digits_3_ipg_end as usize;
    right = (nt_Digits_3_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_4_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_4_ipg_start, mut nt_Digits_4_ipg_end, nt_Digits_4) = match nt_Digits_4_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_4_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_4_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_4_ipg_end);
    }
    nt_Digits_4_ipg_end += left;
    nt_Digits_4_ipg_start += left;
    left = nt_Digits_4_ipg_start;
    right = nt_Digits_4_ipg_end;

    // { minute = Digits@4.value }
    let mut self_minute = nt_Digits_4.value;

    // Digits@5[Digits@4.END, Digits@4.END + 2]
    left = nt_Digits_4_ipg_end as usize;
    right = (nt_Digits_4_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_5_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_5_ipg_start, mut nt_Digits_5_ipg_end, nt_Digits_5) = match nt_Digits_5_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_5_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_5_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_5_ipg_end);
    }
    nt_Digits_5_ipg_end += left;
    nt_Digits_5_ipg_start += left;
    left = nt_Digits_5_ipg_start;
    right = nt_Digits_5_ipg_end;

    // { second = Digits@5.value }
    let mut self_second = nt_Digits_5.value;

    // Digits@6[Digits@5.END, Digits@5.END + 2]
    left = nt_Digits_5_ipg_end as usize;
    right = (nt_Digits_5_ipg_end + 2 as usize) as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_Digits_6_m = Digits(input, begin + left, begin + right);
    let (mut nt_Digits_6_ipg_start, mut nt_Digits_6_ipg_end, nt_Digits_6) = match nt_Digits_6_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_Digits_6_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_Digits_6_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_Digits_6_ipg_end);
    }
    nt_Digits_6_ipg_end += left;
    nt_Digits_6_ipg_start += left;
    left = nt_Digits_6_ipg_start;
    right = nt_Digits_6_ipg_end;

    // { hundrethsOfSecond = Digits@6.value }
    let mut self_hundrethsOfSecond = nt_Digits_6.value;

    // { gmtOffset = .[Digits@6.END] }
    left = nt_Digits_6_ipg_end as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_gmtOffset = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, DateAndTime {
      day: self_day,
      gmtOffset: self_gmtOffset,
      hour: self_hour,
      hundrethsOfSecond: self_hundrethsOfSecond,
      minute: self_minute,
      month: self_month,
      second: self_second,
      year: self_year,
    }));
  }

  return None;
}

fn Digits(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Digits)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat Digit@0[Digit@0.END, EOI].value starting on [0, EOI]
    let mut self_values = Vec::new();
    left = 0 as usize;
    right = EOI as usize;
    let nt_Digit_0_m = Digit(input, begin + left, begin + right);
    let mut nt_Digit_0_ipg_start = right;
    let mut nt_Digit_0_ipg_end = left;
    match nt_Digit_0_m {
      None => {}
      Some((nt_Digit_0_ipg_start_, nt_Digit_0_ipg_end_, nt_Digit_0)) => {
        nt_Digit_0_ipg_start = nt_Digit_0_ipg_start_;
        nt_Digit_0_ipg_end = nt_Digit_0_ipg_end_;
        if nt_Digit_0_ipg_end == 0 { panic!("repeat of non-consuming rule: Digit"); }
        self_ipg_start = self_ipg_start.min(left + nt_Digit_0_ipg_start);
        self_ipg_end = self_ipg_end.max(left + nt_Digit_0_ipg_end);
        nt_Digit_0_ipg_end += left;
        nt_Digit_0_ipg_start += left;
        left = nt_Digit_0_ipg_end as usize;
        right = EOI as usize;
        self_values.push(nt_Digit_0.value);

        while left <= right && right <= EOI {
          let nt_Digit_0_m = Digit(input, begin + left, begin + right);
          let (nt_Digit_0_ipg_start_, nt_Digit_0_ipg_end_, nt_Digit_0) = match nt_Digit_0_m {
            None => { break; }
            Some(p) => p,
          };
          nt_Digit_0_ipg_start = nt_Digit_0_ipg_start_;
          nt_Digit_0_ipg_end = nt_Digit_0_ipg_end_;
          if nt_Digit_0_ipg_end == 0 { panic!("repeat of non-consuming rule: Digit"); }
          self_ipg_start = self_ipg_start.min(left + nt_Digit_0_ipg_start);
          self_ipg_end = self_ipg_end.max(left + nt_Digit_0_ipg_end);
          nt_Digit_0_ipg_end += left;
          nt_Digit_0_ipg_start += left;
          self_values.push(nt_Digit_0.value);
          left = nt_Digit_0_ipg_end as usize;
          right = EOI as usize;
        }
      }
    };

    // { value = decodeAscii2(values) }
    let mut self_value = decodeAscii2(self_values);

    return Some((self_ipg_start, self_ipg_end, Digits {
      value: self_value,
    }));
  }

  return None;
}

fn Digit(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Digit)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { value = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_value = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    // ?[ value == 0 || value >= 48 && value <= 57 ]
    if !(self_value == 0 || self_value >= 48 && self_value <= 57) { break '_ipg_alt; }

    return Some((self_ipg_start, self_ipg_end, Digit {
      value: self_value,
    }));
  }

  return None;
}

fn HexBytes(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, HexBytes)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { bytes = *[0, EOI] }
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_bytes = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = asHex(bytes) }
    let mut self_value = asHex(self_bytes);

    return Some((self_ipg_start, self_ipg_end, HexBytes {
      value: self_value,
    }));
  }

  return None;
}

fn LE_U16(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, LE_U16)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { b = *[0, 2] }
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_b = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (b[1] :: Int) << 8 | (b[0] :: Int) }
    let mut self_value = (self_b[1] as i64) << 8 | (self_b[0] as i64);

    return Some((self_ipg_start, self_ipg_end, LE_U16 {
      value: self_value,
    }));
  }

  return None;
}

fn BE_U16(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BE_U16)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { b = *[0, 2] }
    left = 0 as usize;
    right = 2 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_b = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (b[0] :: Int) << 8 | (b[1] :: Int) }
    let mut self_value = (self_b[0] as i64) << 8 | (self_b[1] as i64);

    return Some((self_ipg_start, self_ipg_end, BE_U16 {
      value: self_value,
    }));
  }

  return None;
}

fn BB_U16(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BB_U16)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // LE_U16@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U16_0_m = LE_U16(input, begin + left, begin + right);
    let (mut nt_LE_U16_0_ipg_start, mut nt_LE_U16_0_ipg_end, nt_LE_U16_0) = match nt_LE_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U16_0_ipg_end);
    }
    nt_LE_U16_0_ipg_end += left;
    nt_LE_U16_0_ipg_start += left;
    left = nt_LE_U16_0_ipg_start;
    right = nt_LE_U16_0_ipg_end;

    // BE_U16@0[LE_U16@0.END, EOI]
    left = nt_LE_U16_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U16_0_m = BE_U16(input, begin + left, begin + right);
    let (mut nt_BE_U16_0_ipg_start, mut nt_BE_U16_0_ipg_end, nt_BE_U16_0) = match nt_BE_U16_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U16_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U16_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U16_0_ipg_end);
    }
    nt_BE_U16_0_ipg_end += left;
    nt_BE_U16_0_ipg_start += left;
    left = nt_BE_U16_0_ipg_start;
    right = nt_BE_U16_0_ipg_end;

    // ?[ LE_U16@0.value == BE_U16@0.value ]
    if !(nt_LE_U16_0.value == nt_BE_U16_0.value) { break '_ipg_alt; }

    // { value = LE_U16@0.value }
    let mut self_value = nt_LE_U16_0.value;

    return Some((self_ipg_start, self_ipg_end, BB_U16 {
      value: self_value,
    }));
  }

  return None;
}

fn LE_U32(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, LE_U32)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { b = *[0, 4] }
    left = 0 as usize;
    right = 4 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_b = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (b[3] :: Int) << 24 | (b[2] :: Int) << 16 | (b[1] :: Int) << 8 | (b[0] :: Int) }
    let mut self_value = (self_b[3] as i64) << 24 | (self_b[2] as i64) << 16 | (self_b[1] as i64) << 8 | (self_b[0] as i64);

    return Some((self_ipg_start, self_ipg_end, LE_U32 {
      value: self_value,
    }));
  }

  return None;
}

fn BE_U32(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BE_U32)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { b = *[0, 4] }
    left = 0 as usize;
    right = 4 as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let mut self_b = (&input[begin + left .. begin + right]).to_vec();
    if left != right {
      self_ipg_start = self_ipg_start.min(left);
      self_ipg_end = self_ipg_end.max(right);
    }

    // { value = (b[0] :: Int) << 24 | (b[1] :: Int) << 16 | (b[2] :: Int) << 8 | (b[3] :: Int) }
    let mut self_value = (self_b[0] as i64) << 24 | (self_b[1] as i64) << 16 | (self_b[2] as i64) << 8 | (self_b[3] as i64);

    return Some((self_ipg_start, self_ipg_end, BE_U32 {
      value: self_value,
    }));
  }

  return None;
}

fn BB_U32(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, BB_U32)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // LE_U32@0[0, EOI]
    left = 0 as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_LE_U32_0_m = LE_U32(input, begin + left, begin + right);
    let (mut nt_LE_U32_0_ipg_start, mut nt_LE_U32_0_ipg_end, nt_LE_U32_0) = match nt_LE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_LE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_LE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_LE_U32_0_ipg_end);
    }
    nt_LE_U32_0_ipg_end += left;
    nt_LE_U32_0_ipg_start += left;
    left = nt_LE_U32_0_ipg_start;
    right = nt_LE_U32_0_ipg_end;

    // BE_U32@0[LE_U32@0.END, EOI]
    left = nt_LE_U32_0_ipg_end as usize;
    right = EOI as usize;
    if right < left || right > EOI { break '_ipg_alt; }
    let nt_BE_U32_0_m = BE_U32(input, begin + left, begin + right);
    let (mut nt_BE_U32_0_ipg_start, mut nt_BE_U32_0_ipg_end, nt_BE_U32_0) = match nt_BE_U32_0_m {
      None => { break '_ipg_alt; }
      Some(p) => p,
    };
    if nt_BE_U32_0_ipg_end != 0 {
      self_ipg_start = self_ipg_start.min(left + nt_BE_U32_0_ipg_start);
      self_ipg_end = self_ipg_end.max(left + nt_BE_U32_0_ipg_end);
    }
    nt_BE_U32_0_ipg_end += left;
    nt_BE_U32_0_ipg_start += left;
    left = nt_BE_U32_0_ipg_start;
    right = nt_BE_U32_0_ipg_end;

    // ?[ LE_U32@0.value == BE_U32@0.value ]
    if !(nt_LE_U32_0.value == nt_BE_U32_0.value) { break '_ipg_alt; }

    // { value = LE_U32@0.value }
    let mut self_value = nt_LE_U32_0.value;

    return Some((self_ipg_start, self_ipg_end, BB_U32 {
      value: self_value,
    }));
  }

  return None;
}

fn NULBytes(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, NULBytes)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // repeat NUL_BYTE@0[NUL_BYTE@0.END, EOI].this starting on [0, EOI]
    let mut self_values = Vec::new();
    left = 0 as usize;
    right = EOI as usize;
    let nt_NUL_BYTE_0_m = NUL_BYTE(input, begin + left, begin + right);
    let mut nt_NUL_BYTE_0_ipg_start = right;
    let mut nt_NUL_BYTE_0_ipg_end = left;
    match nt_NUL_BYTE_0_m {
      None => {}
      Some((nt_NUL_BYTE_0_ipg_start_, nt_NUL_BYTE_0_ipg_end_, nt_NUL_BYTE_0)) => {
        nt_NUL_BYTE_0_ipg_start = nt_NUL_BYTE_0_ipg_start_;
        nt_NUL_BYTE_0_ipg_end = nt_NUL_BYTE_0_ipg_end_;
        if nt_NUL_BYTE_0_ipg_end == 0 { panic!("repeat of non-consuming rule: NUL_BYTE"); }
        self_ipg_start = self_ipg_start.min(left + nt_NUL_BYTE_0_ipg_start);
        self_ipg_end = self_ipg_end.max(left + nt_NUL_BYTE_0_ipg_end);
        nt_NUL_BYTE_0_ipg_end += left;
        nt_NUL_BYTE_0_ipg_start += left;
        left = nt_NUL_BYTE_0_ipg_end as usize;
        right = EOI as usize;
        self_values.push(nt_NUL_BYTE_0);

        while left <= right && right <= EOI {
          let nt_NUL_BYTE_0_m = NUL_BYTE(input, begin + left, begin + right);
          let (nt_NUL_BYTE_0_ipg_start_, nt_NUL_BYTE_0_ipg_end_, nt_NUL_BYTE_0) = match nt_NUL_BYTE_0_m {
            None => { break; }
            Some(p) => p,
          };
          nt_NUL_BYTE_0_ipg_start = nt_NUL_BYTE_0_ipg_start_;
          nt_NUL_BYTE_0_ipg_end = nt_NUL_BYTE_0_ipg_end_;
          if nt_NUL_BYTE_0_ipg_end == 0 { panic!("repeat of non-consuming rule: NUL_BYTE"); }
          self_ipg_start = self_ipg_start.min(left + nt_NUL_BYTE_0_ipg_start);
          self_ipg_end = self_ipg_end.max(left + nt_NUL_BYTE_0_ipg_end);
          nt_NUL_BYTE_0_ipg_end += left;
          nt_NUL_BYTE_0_ipg_start += left;
          self_values.push(nt_NUL_BYTE_0);
          left = nt_NUL_BYTE_0_ipg_end as usize;
          right = EOI as usize;
        }
      }
    };

    return Some((self_ipg_start, self_ipg_end, NULBytes {
    }));
  }

  return None;
}

fn NUL_BYTE(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, NUL_BYTE)> {
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

    return Some((self_ipg_start, self_ipg_end, NUL_BYTE {
    }));
  }

  return None;
}

fn Byte(input: &[u8], begin: usize, end: usize) -> Option<(usize, usize, Byte)> {
  let EOI: usize = end - begin;
  '_ipg_alt: {
    let mut left: usize = EOI; let mut right: usize = 0;
    let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;

    // { value = .[0] }
    left = 0 as usize;
    right = left + 1;
    if right > EOI { break '_ipg_alt; }
    let mut self_value = input[begin + left];
    self_ipg_start = self_ipg_start.min(left);
    self_ipg_end = self_ipg_end.max(right);

    return Some((self_ipg_start, self_ipg_end, Byte {
      value: self_value,
    }));
  }

  return None;
}

use std::fs;

fn main() {
    let input = fs::read("./test/node/samples/1.iso").unwrap();
    println!("{:#?}", ISO9660(&input, 0, input.len()));
}
