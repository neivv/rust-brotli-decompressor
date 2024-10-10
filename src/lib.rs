#![no_std]
#![allow(dead_code, non_snake_case)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]


#[macro_use]
extern crate alloc_no_stdlib as alloc;
use alloc::{AllocatedStackMemory, Allocator, SliceWrapper, SliceWrapperMut, StackAllocator, bzero};
use core::ops;

#[macro_use]
mod memory;
mod dictionary;
mod brotli_alloc;
#[macro_use]
mod bit_reader;
mod huffman;
mod state;
mod prefix;
mod context;
mod transform;
mod test;
mod decode;
mod io_wrappers;
use huffman::{HuffmanCode, HuffmanTreeGroup};
use state::BrotliState;
use decode::{BrotliDecompressStream, BrotliResult, };

declare_stack_allocator_struct!(MemPool, 512, stack);

pub fn brotli_decode_prealloc(
  input: &[u8],
  mut output: &mut[u8],
  scratch_u8: &mut [u8],
  scratch_u32: &mut [u32],
  scratch_hc: &mut [HuffmanCode],
) {
  let stack_u8_allocator = MemPool::<u8>::new_allocator(scratch_u8, bzero);
  let stack_u32_allocator = MemPool::<u32>::new_allocator(scratch_u32, bzero);
  let stack_hc_allocator = MemPool::<HuffmanCode>::new_allocator(scratch_hc, bzero);
  let mut available_out = output.len();
  let mut available_in: usize = input.len();
  let mut input_offset: usize = 0;
  let mut output_offset: usize = 0;
  let mut written: usize = 0;
  let mut brotli_state =
    BrotliState::new(stack_u8_allocator, stack_u32_allocator, stack_hc_allocator);
  ::BrotliDecompressStream(&mut available_in,
                                      &mut input_offset,
                                      &input[..],
                                      &mut available_out,
                                      &mut output_offset,
                                      &mut output,
                                      &mut written,
                                      &mut brotli_state);
}

