#!/usr/bin/env python3

import argparse
import json
import os
import subprocess
from datetime import datetime
from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    TextColumn,
    TimeElapsedColumn,
    TimeRemainingColumn,
)

parser = argparse.ArgumentParser(prog=os.path.basename(__file__))
parser.add_argument(
    "-d", "--result-dir",
    default=os.getcwd(),
    help="set the result directory (default: current working directory)"
)
parser.add_argument(
    "-o", "--output",
    default=None,
    help="set the name of the result JSON file"
)
parser.add_argument(
    "-r", "--nruns",
    type=int,
    default=30,
    help="set the number of samples (default: 30)"
)
args = parser.parse_args()

HERE = os.path.dirname(os.path.abspath(__file__))
TOP  = os.path.join(HERE, "..")
BENCHMARK_DIR = os.path.join(TOP, "benchmarks")
SML = os.path.join(TOP, "bin", "sml")

PROGRAMS = [
    'binary-trees',
    'black-scholes',
    'boyer',
    'cml-sieve',
    'f-arith',
    'iter-pidigits',
    'knuth-bendix',
    'logic',
    'mandelbrot-rat',
    'mandelbrot',
    'mazefun',
    'mc-ray',
    'nbody',
    'nucleic',
    'pidigits',
    'pingpong',
    'safe-for-space',
    'sat',
    'stream-sieve',
    'tsp',
    'twenty-four',
]

progress = Progress(
    TextColumn("[progress.description]{task.description} {task.fields[flag]} {task.fields[step]}"),
    BarColumn(),
    MofNCompleteColumn(),
    TimeElapsedColumn(),
    TimeRemainingColumn(),
    transient=True
)

def placeholder(program, option):
    return {
        "bmark" : program,
        "tag": option,
        "compiles" : [ -1.0,],
        "runs" : [-1.0],
        "alloc" : {
            "nbAlloc": -1,
            "nbPromote" : -1,
            "nGCs" : []
        }
    }


def runbench(task_id, program):
    progress.start_task(task_id)
    runtimes = []
    profiles = []
    for prog_dir, flag in [("programs", []), ("programs64", ["--64"])]:
        progress.update(task_id, flag=prog_dir, step='measure')

        command = ['./run.sh', SML, os.path.join(prog_dir, program + ".sml"), "--tag", prog_dir,
                   "--rm", "--nruns", str(args.nruns)] + flag
        try:
            with open(os.devnull, "wb") as devnull:
                process = subprocess.run(
                    command,
                    stdout=subprocess.PIPE,
                    stderr=devnull,
                    check=True,
                    text=True,
                    cwd=BENCHMARK_DIR
                )
            data = eval(process.stdout)
            runtimes.append(data)
            progress.console.log(data)
        except:
            progress.console.log("FAIL: " + " ".join(command))
            print()
            print()
            print()
            raise

        progress.update(task_id, flag=prog_dir, step='profile')
        _, profile = runprofile(program, prog_dir, flag)
        profiles.append(profile)

    progress.advance(task_id)
    return (program, { 'runtimes': runtimes, 'profiles': profiles })

def runprofile(program, prog_dir, flag):
    command = ['./run.sh', SML, os.path.join(prog_dir, program + ".sml"), "--tag", prog_dir, "--rm",
               "--nruns", "0", "--ncomps", "0", "--instrument"] + flag
    try:
        with open(os.devnull, "wb") as devnull:
            process = subprocess.run(
                command,
                stdout=subprocess.PIPE,
                stderr=devnull,
                check=True,
                text=True,
                cwd=BENCHMARK_DIR
            )
        data = eval(process.stdout)
        progress.console.log(data)
    except:
        progress.console.log("FAIL: " + " ".join(command))
        print()
        print()
        print()
        raise
    return (program, data)

def direct_run():
    results = {}
    for program in PROGRAMS:
        program_task = progress.add_task(
            program, flag="", step="", start=False, total=2
        )
        _, result = runbench(program_task, program)
        results[program] = result
    return results

with progress:
    results = direct_run()

if args.output:
    result_filename = args.output
else:
    result_filename = datetime.now().strftime("benchmark_%Y%m%d_%H%M%S.json")
# result_filename = datetime.now().strftime("benchmark-results.json")

try:
    os.makedirs(args.result_dir, exist_ok=True)
    result_filename = os.path.join(args.result_dir, result_filename)
    with open(result_filename, "w") as result_file:
        json.dump(results, result_file)
except Exception as e:
    print(json.dumps(results))
    raise

