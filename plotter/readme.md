Python scripts based on MatPlotLib for plotting basho bench results for throughput and staleness.

## Usage
Both scripts support drag&drop directories from Finder
```
 $ <script> [<dir1> <dir2> <dir3> ...]
```

## Throughput script

The script's directory (the command line arguments)  should be structured as such
Both 1million and 10 million keyspace plotting is available, as well as exponential, multidc multi- and single-round.

### Changing keyspace and rounds
For *MultiDC* (either multi or single round scenarios) comment out the second line, for *Exponential* scenario, comment out the *MultiDC* line.

```python
e['avg_tput']= float(((avg/3)/10)*((rounds*reads)+writes))/1000000 # MultiDC MultiRound
e['avg_tput']= float(((avg/3)/10)*(1022+writes))/1000000 # For exponential

```

Various other settings are definable in the [```plot(workload)``` function](./throughput.py#L159-L244). Notably at [line 179](./throughput.py#L179) desired write ratios to plot can be defined.

```python
writes = [10, 100]
``` 
Means the script will only grab data with ```writes``` equal either ```10``` or ```100```.

### Tree structure

```bash
$ tree bench-2017-04-12-1492022929-ec/
bench-2017-04-12-1492022929-ec/
├── basho_bench_summary-1000000-10-100-10-1
│   ├── summary.csv
│   ├── summary.png
│   └── txn_latencies.csv
├── basho_bench_summary-1000000-10-100-10-10
│   ├── summary.csv
│   ├── summary.png
│   └── txn_latencies.csv
├── basho_bench_summary-1000000-10-100-10-15
│   ├── summary.csv
│   ├── summary.png
│   └── txn_latencies.csv
├── basho_bench_summary-1000000-10-100-10-20
│   ├── summary.csv
│   ├── summary.png
│   └── txn_latencies.csv
...
```
The dirs fields correspond to the following parameters ```basho_bench_summary-keyspace-rounds-reads-writes-client_threads```.
This result ```basho_bench_summary-11-22-33-44-44-55``` means that the bench was ran with the following parameters:

```
keyspace: 		11
rounds:			22
reads:			33
writes:			44
client_threads:	55
```

Parsing structure is a key value dictionary

```python
workload = {
'type'    # Basho bench or staleness
'keys'    # 1m or 10m
'rounds'  # 10 usually
'reads'   # 100 usually 'writes'  : one from [2, 10, 100]
'threads' # one from [1, 3, 5, 7, 10, 15, 20, 30, 40]
'f_tput'  # summary csv file
'f_lat'   # latency csv file
'tput_avg'# average throughput (calculated as average of 3 values)
'lat_avg' # average latency (calculated as average of 3 values)
}

```

Workload dictionary for plotting

```python
 wload_type = {
 'reads'   : # of reads per round
 'writes'  : # of writes per round
 'threads' : # of threads/clients
 }
```

### Example
![Example throughput plot](./images/singledc.png "Throughput example plot")


## Staleness script
Plots a [CDF](https://en.wikipedia.org/wiki/Cumulative_distribution_function) plot for version staleness. The below example illustrates that for ~99% of reads, the most recent version has been returned for the red curve.

### Tree structure
```bash
$ tree staleness-2017-04-12-1492030851-clocksi/
staleness-2017-04-12-1492030851-clocksi/
├── Stale-1000000-10-100-10-1.csv
├── Stale-1000000-10-100-10-10.csv
├── Stale-1000000-10-100-10-15.csv
├── Stale-1000000-10-100-10-20.csv
├── Stale-1000000-10-100-10-3.csv
├── Stale-1000000-10-100-10-30.csv
├── Stale-1000000-10-100-10-5.csv
├── Stale-1000000-10-100-10-7.csv
├── Stale-1000000-10-100-100-1.csv
├── Stale-1000000-10-100-100-10.csv
├── Stale-1000000-10-100-100-15.csv
├── Stale-1000000-10-100-100-3.csv
...

```
Files follow the same naming convention as for throughput directories, that is:
```Stale-keyspace-rounds-reads-writes-client_threads```

### Example output 
![Example staleness plot](./images/example-staleness-muli-dc-multi-round-phyx-csi.png "Staleness chart")
