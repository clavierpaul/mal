include "reader";
include "printer";
include "utils";

def read_line:
    . as $in
    | label $top
    | input;

def READ:
    read_str;

def EVAL:
    read_form | .value;

def PRINT:
    pr_str;

def rep:
    READ | EVAL |
        if . != null then
            PRINT
        else
            null
        end;

def repl_:
    ("user> " | stderr) |
    (read_line | rep);

def repl:
    {continue: true} | while(
        .continue;
        try {value: repl_, continue: true}
        catch
            if is_jqmal_error then
                {value: "Error: \(.)", continue: true}
            else
                {value: ., continue: false}
            end) | if .value then .value else empty end;

repl
