# Benchmarks

This directory contains the benchmarking infrastructure for measuring the performance of various deque-like data structures implemented in OCaml.

## Overview

The benchmarks measure the execution time of typical operations on different data structures:
- **List**: Standard OCaml lists
- **Sek**: A library providing efficient persistent sequences
- **Deque**: Double-ended queue
- **Steque**: Stack-ended queue (efficient at one end, linear at the other)
- **Cadeque**: Catenable deque (with efficient concatenation)
- **KOT**: Another deque implementation

## How the Benchmark Works

### 1. Database Construction

For each data structure, a database of randomly constructed structures is created:

1. **Raw Database Creation** (`database.ml`):
   - Structures are organized into **bins** based on their size
   - The i-th bin contains structures whose lengths fall within the range [2^(i-1), 2^i)
   - Each bin contains multiple structure instances (inhabitants)
   - Structures are built using random sequences of operations: `push`, `pop`, `inject`, `eject`, and `concat`

2. **Structure Database Construction** (`benchmark.ml`):
   - The raw database (which only stores lengths and operation history) is instantiated for each specific data structure
   - Actual structures are created by replaying the operation history

### 2. Operation Benchmarking

For each data structure, the following operations are benchmarked:

- **Unary operations** (`push`, `pop`, `inject`, `eject`):
  - Each operation is performed multiple times on structures from each bin
  - The number of repetitions depends on the expected cost of the operation
  - Results are aggregated per bin to reduce the number of data points

- **Binary operations** (`concat`):
  - The operation is performed multiple times on structures from bins
  - Two variants are benchmarked:
    - `concat`: Full matrix of all bin pairs (can be disabled due to cost)
    - `concat-diagonal`: Only pairs from the same bin
  - A random subset of structure pairs is used to keep execution time reasonable

### 3. Measurement Details

The measurement (`measure.ml`) results are stored as data points `(time, executions)`:
  - `time`: Total execution time in seconds
  - `executions`: Number of times the operation was performed

## Running the Benchmarks

### Basic Usage

To run the benchmarks with default settings:

```bash
make run
```

**Warning**: This can take several tens of minutes to complete.

### Command-Line Options

The benchmark executable accepts several options:

```bash
dune exec -- bench/benchmark.exe [OPTIONS] [DATA_STRUCTURES...]
```

**Options**:
- `--bins <int>`: Number of size bins (default: 13)
- `--inhabitants <int>`: Number of inhabitants per bin (default: 10)
- `--minor-heap-size <int>`: Minor heap size in megawords (default: 512, i.e., 4GB)
- `--list`: List the known data structures and exit

**Data Structures**: Specify which structures to benchmark (e.g., `List Deque Cadeque`). If none are specified, all structures are benchmarked.

### Examples

Benchmark only Deque and Cadeque with 15 bins:
```bash
dune exec -- bench/benchmark.exe --bins 15 Deque Cadeque
```

List available data structures:
```bash
dune exec -- bench/benchmark.exe --list
```

## Results

### Output Format

Results are saved as CSV files in the `bench/tmp/` directory:

- **File naming**: `<operation>.csv` (e.g., `push.csv`, `pop.csv`, `concat-diagonal.csv`)
- **Format**: Each row corresponds to a bin (or bin pair for binary operations)
- **Columns**: For each data structure, two columns are written:
  - `<Structure>T`: Total execution time in microseconds
  - `<Structure>N`: Total number of operation executions

## Visualizing Results

Results can be visualized using the Jupyter notebook `graphics.ipynb`.

Ensure you have Python with the required packages:
```bash
pip install pandas numpy matplotlib jupyter
```

