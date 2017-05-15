## Intro
This scripts were developed in order to easily dump cache and snapshot tables from [Antidote](https://github.com/SyncFree/antidote) to a file. Script is split into two parts ```cache_dump.erl``` and ```snapshot_dump.erl```, that each do their own part of the job.
## Dependencies
### [Antidote](https://github.com/SyncFree/antidote)
Stating the obvious

### [ETS - Erlang Term Storage](http://erlang.org/doc/man/ets.html)
The script uses RPCs and pulls information from Erlang's built-in storage tables used to store the caches and snapshots

### [Erlang Remote Procedure Calls](http://erlang.org/doc/man/rpc.html)
All of the interaction with Antidote is executed via RPC calls

## Antidote Tables
#### Available tables discovery
All the calls are executed with RPC inside the script, but will be explained as local calls for simplicity's sake. Example outputs have also been shortened, to keep this readme concise.

We use ```ets:all()``` to retrieve a list of all the tables available at the node we're connecting to, example output should yield something like this:

```erlang
> ets:all().
> [69927303,8978822,
'snapshot_cache-1438665674247607560106752257205091097473808596992',
'ops_cache-1438665674247607560106752257205091097473808596992',
'snapshot_cache-1415829711164312202009819681693899175291684651008',
'ops_cache-1415829711164312202009819681693899175291684651008',
'snapshot_cache-1392993748081016843912887106182707253109560705024'
|...]
```
Alternatively, ```ets:i()``` can be used to obtain more details about tables. Additional informations compared to ```ets:all()``` call include ```id, type, size, mem, owner```.

```erlang
> ets:i().
> id              name              type  size   mem      owner
 ----------------------------------------------------------------------------
 2031729         committed_tx      set   1      314      <0.1580.0>
 'snapshot_cache-502391187832497878132516661246222288006726811648'
 'snapshot_cache-502391187832497878132516661246222288006726811648' set   1    504    <0.1966.0>
...
```
#### Table information
The [info](http://erlang.org/doc/man/ets.html#info-1) function - ```ets:info(Tab)``` - displays detailed information about table ```Tab```.

```erlang
> ets:info('ops_cache-502391187832497878132516661246222288006726811648').
>[{read_concurrency,true},
 {write_concurrency,false},
 {compressed,false},
 {memory,2922},
 {owner,<0.1966.0>},
 {heir,none},
 {name,'ops_cache-502391187832497878132516661246222288006726811648'},
 {size,1},
 {node,'antidote@127.0.0.1'},
 {named_table,true},
 {type,set},
 {keypos,1},
 {protection,protected}]
```
#### Reading a table
The [tab2list](http://erlang.org/doc/man/ets.html#tab2list-1) function - 
```ets:tab2list(Tab)``` - comfortably presents the table contents on screen (our particular output is explained a bit later)

```erlang
> ets:tab2list('ops_cache-502391187832497878132516661246222288006726811648').
[{{my_counter,my_bucket},
  {29,50},
  29,
  {1,
   {clocksi_payload,{my_counter,my_bucket},
                    antidote_crdt_counter,1,
                    {dict,1,16,16,8,80,48,
                          {[],[],[],[],[],[],[],[],[],...},
                          {{[],[],[],[],[],[],[],...}}},
                    {{'antidote@127.0.0.1',{1489,496827,67871}},
                     1489504668107078},
                    {tx_id,1489504668106333,<0.5317.0>}}},
  {2,
   {clocksi_payload,{my_counter,my_bucket},
                    antidote_crdt_counter,1,
                    {dict,1,16,16,8,80,48,
                          {[],[],[],[],[],[],[],[],...},
                          {{[],[],[],[],[],[],...}}},
                    {{'antidote@127.0.0.1',{1489,496827,67871}},
                     1489504655615008},
                    {tx_id,1489504655614713,<0.5307.0>}}},
…}]

```

#### Table to file
The [tab2file](http://erlang.org/doc/man/ets.html#tab2file-2) function - 
```ets:tab2file(Tab, Filename)``` dumps table ```Tab``` to file ```Filename``` in binary format.

```erlang
> ets:tab2file('ops_cache-502391187832497878132516661246222288006726811648', 'outputFile.ets').
```

#### File to table
The [file2tab](http://erlang.org/doc/man/ets.html#file2tab-2) - ```ets:file2tab(Filename, Options)``` - function reads a file produced by ```tab2file``` and creates the corresponding table. 

```erlang
> ets:file2tab('outputFile.ets', [{verify, true}]).
{ok,'ops_cache-502391187832497878132516661246222288006726811648'}
```

## Understanding the contents
To understand the complete log structure, refer to [the main readme](../readme.md).

Supposing the following interaction with Antidote

```erlang 
CounterObj = {my_counter, antidote_crdt_counter, my_bucket},
CounterVal = rpc:call(Node, antidote, read_objects, [ignore, [], [CounterObj]]),
{ok, CT}  = rpc:call(Node, antidote, update_objects, [ignore, [], [{CounterObj, increment, 1}]])
```
### Snapshot cache contents
#### Contents
```erlang
> ets:tab2list('snapshot_cache-502391187832497878132516661246222288006726811648').
[{{my_counter,my_bucket},
  {[{{dict,0,16,16,8,80,48,
           {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
           {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},
     {materialized_snapshot,0,0}}],
   1}}]
Ok
```
#### What's what
```erlang
key, snapshot_time, 
materialized_snapshot {
	last_op_id :: op_num = 0, 
	value :: snapshot = 0
}
```

### Operations cache contents
#### Contents
```erlang
> ets:tab2list('ops_cache-502391187832497878132516661246222288006726811648').
[{{my_counter,my_bucket},
  {1,50},
  1,
  {1,
   {clocksi_payload,{my_counter,my_bucket},
                    antidote_crdt_counter,1,
                    {dict,1,16,16,8,80,48,
                          {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
                          {{[[{'antidote@127.0.0.1',{1490,186897,598677}}|
                              1490186922302506]],
                            [],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}},
                    {{'antidote@127.0.0.1',{1490,186897,598677}},
                     1490186922302997},
                    {tx_id,1490186922302506,<0.3610.0>}}},
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}]
Ok
```
#### Pattern deconstruct
```erlang
clocksi_payload {
	key={my_counter,my_bucket}
	type=antidote_crdt_counter
	op_param=1
	snapshot_time={dict,1,16,16,8,80,48, {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}, {{[[{'antidote@127.0.0.1',{1490,186897,598677}}|1490186922302506]],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
	commit_time {
		dcid={'antidote@127.0.0.1',{1490,186897,598677}}
		clock_time=1490186922302997
	}
	tx_id {
		local_start_time=1490186922302506, 
		server_pid=<0.3610.0>
	}
}
```

### OpID Table
#### Contents
```erlang
> ets:tab2list(6226140). 
[{{[502391187832497878132516661246222288006726811648],
   {'antidote@127.0.0.1',{1490,186897,598677}}},
  {op_number,
  	{'antidote@127.0.0.1',
  		{'antidote@127.0.0.1',
  			{1490,186897,598677}}},3,3}},
 {{[502391187832497878132516661246222288006726811648],
   undefined,
  {'antidote@127.0.0.1',
   		{1490,186897,598677}}},
  {op_number,
  	{'antidote@127.0.0.1',
  		{'antidote@127.0.0.1',{1490,186897,598677}}},
             1,1}}]          
```
#### What's what
```erlang
key_hash= 502391187832497878132516661246222288006726811648,
op_number {
		node {
      		node = 'antidote@127.0.0.1', 
    		dcid {
      			node = 'antidote@127.0.0.1'
 	     		tuple = { 1490, 186897, 598677 
   		 	}
  		}
},
bucket_op_number = 
    {op_number { 
      node {
        node = 'antidote@127.0.0.1', 
        dcid {
          node = 'antidote@127.0.0.1'
          tuple = { 1490, 186897, 598677 }
        }
      },
    global = 3, 
    local = 3
}
```
### Commited TXs table
#### Contents
```erlang
> ets:tab2list(2031729). 
[{{my_counter,my_bucket},1490186922302997}]
```
#### What's what
```erlang
key {mycounter, mybucket}
timestamp = 1490186922302997
```

## Usage
Assuming an Antidote instance is running at localhost `127.0.0.1`, registered under name ```antidote```, and your target dump directory is `./dump_dir/`
(which will be created if it doesn't exist)

**Important:** The trailing ```/``` in the dump directory's name is **necessary**. ```./dump_dir``` won't work, as it references a file according to unix conventions.


```bash
$ cache_dump.erl 'antidote@127.0.0.1', "./dump_dir/"
```