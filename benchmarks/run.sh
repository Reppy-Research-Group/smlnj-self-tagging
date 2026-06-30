#!/usr/bin/env bash
#
# usage:
#	run.sh [ --llvm ] prog
#

set -e

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

function usage {
  cat >&2 <<EOF
Usage: $0 SML_EXEC PROG [-o FILE|--output FILE] [--tag LABEL] [-- SML_FLAGS...]

Arguments:
  SML_EXEC             Path to the SML compiler
  PROG                 Path to the program file to run
                       - If PROG contains '/', it is treated as a path,
                         and the working directory of SML will be set to its directory prefix.
                       - Otherwise, PROG is assumed to reside in the same directory
                         as this script.

Options:
  -o, --output FILE    Specify an output file (optional, default: prog-data)
  --tag LABEL          Specify a tag/label for this run (optional, default: unknown)
  -c, --ncomps N       Number of compilation measurements (default: 3)
  -n, --nruns N        Number of runtime measurements (default: 20)
  -i, --instrument     Run instrumentation code (optional, default: off)
  -r, --rm             Remove output file after execution (optional, default: off)
  --                   Separator; remaining arguments are passed verbatim to SML

Example:
  $0 sml prog.sml -o result.txt --tag test1 -c 5 -n 10 -- @SMLload=foo.cm
  $0 sml examples/foo/bar.sml -- @SMLload=bar.cm
EOF
}

if [ "$#" -lt 2 ]; then
  usage
  exit 1
fi

SML=$1
prog=$2
shift 2

# Defaults
outfile="$(basename "${prog}" .sml)-data"
tag="unknown"
ncomps=0
nruns=5
instrument=false
remove_output=false
sml_flags=()
default64=false

# Parse optional arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      if [[ -z "$2" || "$2" =~ ^- ]]; then
        echo "Error: Missing output file after '$1'" >&2
        exit 1
      fi
      outfile=$2
      shift 2
      ;;
    --tag)
      if [[ -z "$2" || "$2" =~ ^- ]]; then
        echo "Error: Missing tag after '--tag'" >&2
        exit 1
      fi
      tag=$2
      shift 2
      ;;
    -c|--ncomps)
      if ! [[ "$2" =~ ^[0-9]+$ ]]; then
        echo "Error: Argument for '$1' must be an integer" >&2
        exit 1
      fi
      ncomps=$2
      shift 2
      ;;
    -n|--nruns)
      if ! [[ "$2" =~ ^[0-9]+$ ]]; then
        echo "Error: Argument for '$1' must be an integer" >&2
        exit 1
      fi
      nruns=$2
      shift 2
      ;;
    --64)
      default64=true
      shift
      ;;
    -i|--instrument)
      instrument=true
      shift
      ;;
    -r|--rm)
      remove_output=true
      shift
      ;;
    --)
      shift
      sml_flags=("$@")
      break
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    -*)
      echo "Error: Unknown option '$1'" >&2
      usage
      exit 1
      ;;
    *)
      echo "Error: Unexpected argument '$1'" >&2
      usage
      exit 1
      ;;
  esac
done

# Determine program path and working directory
if [[ "$prog" == */* ]]; then
  workdir=$(dirname "$prog")
  progfile=$(basename "$prog")
else
  workdir="$HERE"
  progfile="$prog"
fi
progname=$(basename "$progfile" .sml)

# Validate SML
SML=$(command -v "$SML" || true)
if [[ -z "$SML" ]] || [[ ! -x "$SML" ]]; then
  echo "Error: SML not found or not executable." >&2
  exit 1
fi
# Absolute path
SML=$(cd "$(dirname "$SML")" && pwd)/$(basename "$SML")


# Validate benchmark program
if [[ ! -f "${workdir}/${progfile}" ]]; then
  echo "Error: Program file '${workdir}/${progfile}' does not exist." >&2
  exit 1
fi

cd "$workdir"

echo "Running ${SML} in ${workdir} with results in ${outfile}." >&2

echo "{" > ${outfile}
echo " \"bmark\" : \"${progname}\", " >> "${outfile}"

# first we time the compile time
#
if [ "$ncomps" -gt 0 ]; then
  echo "    compiling ..." >&2
  echo -n " \"compiles\" : " >> "${outfile}"
  "$SML" "${sml_flags[@]}" <<EOF >&2
    use "profileit.sml";
    val outS = TextIO.openAppend("${outfile}");
    fun loop 0 = (TextIO.output (outS, "],\n"))
      | loop i = (
          Timing.timeUse (outS, "${progfile}");
          TextIO.output(outS, ",");
          loop (i-1));
    TextIO.output (outS, "[");
    loop ${ncomps};
    TextIO.flushOut outS;
    TextIO.closeOut outS;
EOF
fi

# then measure runtimes
#
if [ "$nruns" -gt 0 ]; then
  echo "    running ..." >&2
  "$SML" "${sml_flags[@]}" <<EOF >&2
    use "profileit.sml";
    Control.Elab.default64 := ${default64};
    Control.overloadKW := ${default64};
    use "${progfile}";
    Control.Elab.default64 := false;
    val outS = TextIO.openAppend("${outfile}");
    Timing.time(${nruns}, outS, Main.doit);
    TextIO.flushOut outS;
    Measuring.measure(outS, Main.doit);
    TextIO.flushOut outS;
    TextIO.closeOut outS;
EOF
fi

if [ "$instrument" = true ]; then
  "$SML" <<EOF >&2
    use "profileit.sml";
    Control.CG.instrument := true;
    Control.overloadKW := true;
    Control.Elab.default64 := ${default64};
    use "${progfile}";
    Control.Elab.default64 := false;
    Control.CG.instrument := false;
    val outS = TextIO.openAppend("${outfile}");
    Profiling.profile (outS, Main.doit);
    TextIO.flushOut outS;
    TextIO.closeOut outS;
EOF
fi
echo " \"tag\": \"${tag}\"" >> "$outfile"
echo "}" >> "$outfile"

cat "$outfile"
if [ "$remove_output" = true ] && [ -n "$outfile" ]; then
  rm -f "$outfile"
fi

