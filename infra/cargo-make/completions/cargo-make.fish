# Cargo owns completion for Cargo syntax, workspace packages, and targets.
# Rumoca adds only its public cargo-make tasks and grouped command words.
if command --query cargo
    CARGO_COMPLETE=fish cargo | source
end

set --local __rumoca_completion_dir (path dirname (status filename))
set --local __rumoca_repo_root (path resolve "$__rumoca_completion_dir/../../..")
set --global __rumoca_cargo_make_task_names \
    (awk '
        function emit() { if (task != "" && !private) print task }
        /^\[tasks\.[^]]+\]$/ {
            emit()
            task = substr($0, 8, length($0) - 8)
            private = 0
            next
        }
        /^\[/ { emit(); task = ""; private = 0; next }
        task != "" && /^[[:space:]]*private[[:space:]]*=[[:space:]]*true/ { private = 1 }
        END { emit() }
    ' \
        "$__rumoca_repo_root/Makefile.toml" \
        "$__rumoca_repo_root"/infra/cargo-make/*.toml \
        | env LC_ALL=C sort -u)
set --global __rumoca_cargo_make_command_lines \
    (awk '
        /^#[[:space:]]*completion-words:[[:space:]]*/ {
            words = $0
            sub(/^#[[:space:]]*completion-words:[[:space:]]*/, "", words)
            next
        }
        /^\[tasks\.[^]]+\]$/ && words != "" {
            task = substr($0, 8, length($0) - 8)
            print task "\t" words
            words = ""
            next
        }
        /^[^#[:space:]]/ { words = "" }
    ' \
        "$__rumoca_repo_root/Makefile.toml" \
        "$__rumoca_repo_root"/infra/cargo-make/*.toml)

function __rumoca_cargo_make_tasks
    string join ' ' $__rumoca_cargo_make_task_names
end

function __rumoca_cargo_make_needs_task
    set --local tokens (commandline -opc)
    test (count $tokens) -eq 2
    and test "$tokens[1]" = cargo
    and test "$tokens[2]" = make
end

function __rumoca_cargo_make_needs_command_word
    set --local tokens (commandline -opc)
    test (count $tokens) -eq 3
    and test "$tokens[1]" = cargo
    and test "$tokens[2]" = make
end

function __rumoca_cargo_make_command_words
    set --local tokens (commandline -opc)
    set --local task "$tokens[3]"
    for line in $__rumoca_cargo_make_command_lines
        set --local fields (string split --max 1 \t -- "$line")
        if test "$fields[1]" = "$task"
            string split ' ' -- "$fields[2]"
            return
        end
    end
end

complete --command cargo \
    --condition '__rumoca_cargo_make_needs_task' \
    --no-files \
    --arguments '(__rumoca_cargo_make_tasks)'
complete --command cargo \
    --condition '__rumoca_cargo_make_needs_command_word' \
    --no-files \
    --arguments '(__rumoca_cargo_make_command_words)'
