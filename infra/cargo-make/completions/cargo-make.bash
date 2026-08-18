# Cargo owns completion for Cargo syntax, workspace packages, and targets.
# Rumoca adds only its public cargo-make tasks and grouped command words.
if ! declare -F _clap_complete_cargo >/dev/null && command -v cargo >/dev/null 2>&1; then
    source <(CARGO_COMPLETE=bash cargo)
fi
if ! declare -F _clap_complete_cargo >/dev/null && ! declare -F _cargo >/dev/null; then
    if command -v rustup >/dev/null 2>&1; then
        source <(rustup completions bash cargo)
    fi
fi

_rumoca_cargo_make_completion_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
_rumoca_cargo_make_root="$(cd -- "${_rumoca_cargo_make_completion_dir}/../../.." && pwd)"
_rumoca_cargo_make_files=(
    "${_rumoca_cargo_make_root}/Makefile.toml"
    "${_rumoca_cargo_make_root}"/infra/cargo-make/*.toml
)
_RUMOCA_CARGO_MAKE_TASKS="$(
    awk '
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
        "${_rumoca_cargo_make_files[@]}" \
        | LC_ALL=C sort -u
)"

while IFS=$'\t' read -r _rumoca_task _rumoca_words; do
    _rumoca_variable="_RUMOCA_CARGO_MAKE_WORDS_${_rumoca_task//-/_}"
    printf -v "${_rumoca_variable}" '%s' "${_rumoca_words}"
done < <(
    awk '
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
    ' "${_rumoca_cargo_make_files[@]}"
)
unset _rumoca_cargo_make_completion_dir _rumoca_cargo_make_root
unset _rumoca_cargo_make_files _rumoca_task _rumoca_words _rumoca_variable

_rumoca_cargo_completion()
{
    if [[ ${COMP_CWORD} -eq 2 && "${COMP_WORDS[1]}" == "make" ]]; then
        local current
        current="${COMP_WORDS[COMP_CWORD]}"
        COMPREPLY=($(compgen -W "${_RUMOCA_CARGO_MAKE_TASKS}" -- "${current}"))
        return
    fi

    if [[ ${COMP_CWORD} -eq 3 && "${COMP_WORDS[1]}" == "make" ]]; then
        local current task variable words
        current="${COMP_WORDS[COMP_CWORD]}"
        task="${COMP_WORDS[2]}"
        variable="_RUMOCA_CARGO_MAKE_WORDS_${task//-/_}"
        words="${!variable-}"
        if [[ -n "${words}" ]]; then
            COMPREPLY=($(compgen -W "${words}" -- "${current}"))
            return
        fi
    fi

    if [[ ${COMP_CWORD} -gt 3 && "${COMP_WORDS[1]}" == "make" ]]; then
        local current
        current="${COMP_WORDS[COMP_CWORD]}"
        COMPREPLY=($(compgen -W "--help" -- "${current}"))
        return
    fi

    if declare -F _clap_complete_cargo >/dev/null; then
        _clap_complete_cargo "$@"
    elif declare -F _cargo >/dev/null; then
        _cargo "$@"
    fi
}

if [[ "${BASH_VERSINFO[0]}" -gt 4 ]] \
    || [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 ]]; then
    complete -o nospace -o bashdefault -o default -o nosort -F _rumoca_cargo_completion cargo
else
    complete -o nospace -o bashdefault -o default -F _rumoca_cargo_completion cargo
fi
