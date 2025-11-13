## Eincalc Sheet (pre-alpha)

![Screenshot](eincalc.png "screenshot")

Eincalc is spreadsheet engine modeled around basic tensor operations targeting
mainly the web. Unlike most spreadsheet engines the formulae are separated from 
the grid for easier management. At the moment it supports. 

- [x] Calculations
    - [x] Arithmetic operations
    - [x] Einsum evaluation
- [x] Formulaes
    - [x] Rescaling 
        - [x] MinMax
        - [x] Zscore
    - [x] Tendencies
        - [x] Mean
        - [x] Stddev
        - [x] Sum
        - [x] Cumulative Summing
    - [x] Reshaping
    - [x] Slicing
    - [x] Axis based operations (e.g Mean along an axis)
    - [x] Reading and Writing values to cells
- [ ] 🚧 Draw and Plotting
- [ ] 🚧 Javascript and OCaml API 
- [ ] 🚧 Headless support (with Canvas or Svg)
- [ ] 🚧 Generator functions (not yet lazy)
    - [x] Random number 
    - [x] Diagonal matrices
    - [x] Ones, Zeros, Fill or Alternating values
- [ ] 🚧 Serialization 
- [ ] 🚧 Cell Reactivity
- [ ] 🚧 Custom data types
    - [x] Float values | arrays
    - [ ] Complex numbers 
    - [ ] Custom data types
    - [ ] 🚧 Text embedding
    - [ ] 🚧 Streams
        - [ ] Images 
        - [ ] Video 
        - [ ] Audio
- [ ] 🚧 [WebNN](https://www.w3.org/TR/webnn/)
- [ ] 🚧  OCR and gen-AI ??

### Building the project

The main project is in Ocaml which can be interfaced via a repl on the command
line and a web interface.

#### Requirements

 * Ocaml version 5.0+
 * Dune (Ocaml build system) and Js of Ocaml installed via opam
 * Bun js (for the Svelte frontend)

To build and run the repl:

```bash 
dune build --profile release
dune exec  --profile release eincalc
```

To use the frontend

```bash 
dune build --profile release && cd ./bin/web && bun i && bun dev
```

To use in Utop (isolate Jsoo code in /bin/web)

```bash
dune build utop
```

### Formulae

##### Tutorial 

These functions can be executed in the repl or the formula input of the web
version. 


> You can use these functions in any cell or in the formulae
> box. At the moment cell changes are not reactive so changing dependant cells
> will not reflect. Also formulaes are NOT stored in the cells (for now). These
> features will be added in the future.  


> When using in a cell, you MUST start the cell with an `=`  sign otherwise it
> is interpreted directly as either text or number


You can use generators to initialize cells with data too. Here are a few.

- Random numbers with an upper bound of 100, reshaped into a 5x5 matrix
```
=@rand<100,[5,5]>
```

- Enumarate from 0 with 10 step increments into a 5-length vector
```
=@enum<0,10,[5]>
```

- Fill with a value into a 5 length vector
```
=@fill<0,10,[5]>
```

- Diagonal 4x4 matrix with 2 along the diagonal
```
=@diag<2,4>
```

- Add a list of values directly (only numbers supported for now, text arrays
will fail until future release!)
```
=[1,2,3,4,5,6,7,8,9,10]
```

Lets Add 2 numbers.

```
 =2+2 
```

Compute result and write to cell A1. (Parantheses aid in clarity of the
expression)

```
 =((6 / 3) + 3) | write<A1>
```

Multiply 2 Arrays and write result to B2. (Note that tensor result will be
interpreted in Row major form whilst being written to the grid). Eincalc will
check whether the operations can be broadcasted together

```
 =([ 1,2,3 ] * [ 4, 5, 6 ]) | write<B2>
```

Add 5 to each element fetched from the grid and write back to the grid

```
 =((@B2..D2) + 5)
```

Or write it back in reverse (the start and end cells in the range are swapped
for this effect) 

```
 =((@D2..B2) + 5)
```

You can use einsum to transpose a matrix, multiply all elements by 3 and write it to the grid 

```
 = ((ij -> ji, [[[1,2,3]],[[4,5,6]]]) | write<B6>
```

> PS: You cannot evaluate expressions within an einsum expression at the moment

Generate random matrices  and multiply them using einsum then write them back

```
 =(ij, jk -> ik, @rand<100,[6,6]>, @rand<100,[6,6]>) | write<A1>
```

Same as above but with z-score normalization. The masks can be used within
einsum expression but math expressions don't work.

```
=(ij, jk -> ik, @rand<100,[6,6]> | zscore, @rand<100,[6,6]> | zscore) | write<A1>
```

Same as above but we slice the lower right half. 

```
=(ij, jk -> ik, @rand<100,[6,6]> | zscore, @rand<100,[6,6]> | zscore) | slice<[3::, 3::]> | write<A1>
```



Plotting and Drawing some values (Still needs a lot of work)
```
=(([[1,2,3,4,5,6,7,8,9,10],[10,20,30,40,50,60,70,80,90,100]] | plot<handle, [320,240], scatter<[0:1:, ::],[1:1:, ::], {r=5,c=blue}>> | drawall<handle, [350,290], [ line{x=0,fx=160,y=0,fy=120,c=green}, circle{x=160,y=120,r=30,c=white,b=1}, text{x=150,y=120,t='Hallo Chart!',s=16,c=red} ]> )) | write<C14>
```

## More to come

- Charting API
- Function call API (cli use) against incoming data stream
- More einsum operators
- Advanced data types (Video, Images, Audio, Text, Custom types ..etc)
- OCR support
- Logic
