// rustc.exe +nightly --edition=2015 src/lib.rs  --crate-type lib  -C opt-level=1 --target i686-pc-windows-msvc
#![no_std]
#![allow(dead_code, non_snake_case)]
#![allow(unused_parens)]
#![allow(unused_imports)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

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

pub fn bzero<T : Default> (data : &mut [T]) {
    for iter in data.iter_mut() {
        *iter = T::default();
    }
}

pub trait Allocator<T> {
    type AllocatedMemory : AllocatedSlice<T>;
    fn alloc_cell(&mut self, len : usize) -> Self::AllocatedMemory;
    fn free_cell(&mut self, data : Self::AllocatedMemory);
}

pub struct AllocatedStackMemory<'a, T:'a> {
    pub mem : &'a mut [T],
}


impl<'a, T: 'a> core::default::Default for AllocatedStackMemory<'a, T> {
    fn default() -> Self {
        return AllocatedStackMemory::<'a, T>{mem : &mut[]};
    }
}

impl<'a, T: 'a> SliceWrapper<T> for AllocatedStackMemory<'a, T> {
    fn slice(& self) -> & [T] {
        return & self.mem;
    }
}

impl<'a, T: 'a> SliceWrapperMut<T> for AllocatedStackMemory<'a, T> {
    fn slice_mut(& mut self) ->& mut [T] {
        return &mut self.mem;
    }
}

pub trait SliceWrapper<T> {
    fn slice(& self) -> & [T];
    fn len(&self) -> usize{
        self.slice().len()
    }
}

pub trait SliceWrapperMut<T> : SliceWrapper<T> {
  fn slice_mut (&mut self) -> & mut [T];
}


pub trait AllocatedSlice<T>
    : SliceWrapperMut<T> + SliceWrapper<T> + Default {
}

impl<T, U> AllocatedSlice<T> for U where U : SliceWrapperMut<T> + SliceWrapper<T> + Default {

}

pub struct StackAllocator<'a,
                           T :'a,
                           U : AllocatedSlice<&'a mut [T]>> {
    pub nop : &'a mut [T],
    pub system_resources : U,
    pub free_list_start : usize,
    pub free_list_overflow_count : usize,
    pub initialize : fn(&mut[T]),
}

impl <'a, T : 'a, U : AllocatedSlice<&'a mut[T]> >
     StackAllocator <'a, T, U> {
    fn clear_if_necessary(self : &Self, index : usize, data : AllocatedStackMemory<'a, T>)
    -> AllocatedStackMemory<'a, T> {
        if index + 1 != self.system_resources.slice().len() {
            let fnp = self.initialize;
            fnp(data.mem);
        }
        return data;
    }
}
impl<'a, T : 'a, U : AllocatedSlice<&'a mut[T]> >
    Allocator<T> for StackAllocator <'a, T, U> {
    type AllocatedMemory = AllocatedStackMemory<'a, T>;
    fn alloc_cell(self : &mut StackAllocator<'a, T, U>,
                  len : usize) -> AllocatedStackMemory<'a, T> {
        if len == 0 {
            return AllocatedStackMemory::<'a, T>::default();
        }
        let mut index : usize = self.free_list_start;
        let mut found : bool = false;
        for free_resource in self.system_resources.slice()[self.free_list_start..].iter() {
            if free_resource.len() >= len {
                found = true;
                break;
            }
            index += 1;
        }
        if !found {
            panic!("OOM");
        }
        let available_slice = core::mem::replace(&mut self.system_resources.slice_mut()[index],
                                                    &mut[]);
        if available_slice.len() == len
           || (available_slice.len() < len + 32
               && index + 1 != self.system_resources.slice().len()) {
            // we don't want really small wasted slices
            // we must assign free_list_start
            if index != self.free_list_start {
                assert!(index > self.free_list_start);
                let farthest_free_list = core::mem::replace(
                    &mut self.system_resources.slice_mut()[self.free_list_start],
                    &mut []);
                let _ = core::mem::replace(&mut self.system_resources.slice_mut()[index],
                                   farthest_free_list);
            }
            self.free_list_start += 1;
            return self.clear_if_necessary(index,
                                           AllocatedStackMemory::<'a, T>{mem:available_slice});
        } else { // the memory allocated was not the entire range of items. Split and move on
            let (retval, return_to_sender) = available_slice.split_at_mut(len);
            let _ = core::mem::replace(&mut self.system_resources.slice_mut()[index], return_to_sender);
            return self.clear_if_necessary(index, AllocatedStackMemory::<'a, T>{mem:retval});
        }
    }
    fn free_cell(self : &mut StackAllocator<'a, T, U>,
                 val : AllocatedStackMemory<'a, T>) {
        if val.slice().len() == 0 {
            return;
        }
        if self.free_list_start > 0 {
            self.free_list_start -=1;
            let _ = core::mem::replace(&mut self.system_resources.slice_mut()[self.free_list_start],
                               val.mem);

        } else {
            for _i in 0..3 {
               self.free_list_overflow_count += 1;
               self.free_list_overflow_count %= self.system_resources.slice().len();
               if self.system_resources.slice()[self.free_list_overflow_count].len() < val.mem.len() {
                   let _ = core::mem::replace(&mut self.system_resources.slice_mut()[self.free_list_overflow_count],
                                      val.mem);
                   return;
               }
            }
        }
    }
}

macro_rules! static_array2 {
    (@accum (0, $($_ignored:expr),*) -> ($($body:tt)*))
        => {static_array2!(@as_expr [$($body)*])};
    (@accum (1, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (0, $($expr),*) -> ($($body)* $($expr,)*))};
    (@accum (2, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (0, $($expr),*) -> ($($body)* $($expr,)* $($expr,)*))};
    (@accum (4, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (2, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (8, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (4, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (16, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (8, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (32, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (16, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (64, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (32, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (128, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (64, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (256, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (128, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (512, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (256, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (1024, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (512, $($expr,)* $($expr),*) -> ($($body)*))};
    (@accum (2048, $($expr:expr),*) -> ($($body:tt)*))
        => {static_array2!(@accum (1024, $($expr,)* $($expr),*) -> ($($body)*))};
    (@as_expr $expr:expr) => {$expr};

    ($expr:expr; $n:tt) => { static_array2!(@accum ($n, $expr) -> ()) };
}

macro_rules! define_stack_allocator_traits(
    ($name : ident, $freelist_size : tt, stack) => {
        impl<'a, T: 'a> Default for $name<'a, T> {
            fn default() -> Self {
                return $name::<'a, T>{freelist : static_array2!(&mut[]; $freelist_size)};
            }
        }
        define_stack_allocator_traits!($name, generic);
    };
    ($name : ident, generic) => {
        impl<'a, T: 'a> SliceWrapper<&'a mut[T]> for $name<'a, T> {
            fn slice(& self) -> & [&'a mut[T]] {
                return & self.freelist;
            }
        }
        impl<'a, T: 'a> SliceWrapperMut<&'a mut [T]> for $name<'a, T> {
            fn slice_mut(& mut self) ->&mut [&'a mut [T]] {
                return &mut self.freelist;
            }
        }
        impl<'a, T: 'a> ops::Index<usize> for $name<'a, T> {
            type Output = [T];
            fn index<'b> (&'b self, _index : usize) -> &'b [T] {
                return &self.freelist[_index];
            }
        }

        impl<'a, T: 'a> ops::IndexMut<usize> for $name<'a, T> {
            fn index_mut<'b>(&'b mut self, _index : usize) -> &'b mut [T] {
                return &mut self.freelist[_index];
            }
        }
    };
);

macro_rules! declare_stack_allocator_struct(
    (@as_expr $expr : expr) => {$expr};
    (@new_method $name : ident, $freelist_size : tt) => {
        impl<'a, T: 'a> $name<'a, T> {
          fn new_allocator(global_buffer : &'a mut [T],
                           initializer : fn(&mut[T])) -> StackAllocator<'a, T, $name<'a, T> > {
              let mut retval = StackAllocator::<T, $name<T> > {
                  nop : &mut [],
                  system_resources : $name::<T>::default(),
                  free_list_start : declare_stack_allocator_struct!(@as_expr $freelist_size),
                  free_list_overflow_count : 0,
                  initialize : initializer,
              };
              retval.free_cell(AllocatedStackMemory::<T>{mem:global_buffer});
              return retval;
          }
        }
    };

    ($name :ident, $freelist_size : tt, stack) => {
        struct $name<'a, T : 'a> {
            freelist : [&'a mut [T];declare_stack_allocator_struct!(@as_expr $freelist_size)],
            // can't borrow here: make it on stack-- heap : core::cell::RefCell<[T; $heap_size]>
        }
        define_stack_allocator_traits!($name,
                                       $freelist_size,
                                       stack);
        declare_stack_allocator_struct!( @new_method $name, $freelist_size);
    };
);

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

