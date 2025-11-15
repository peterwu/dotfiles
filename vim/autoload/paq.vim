vim9script

const pack = {
    name:      'plugged',
    start_dir: $'{$VIM_DATA_HOME}/pack/plugged/start',
    opt_dir:   $'{$VIM_DATA_HOME}/pack/plugged/opt'
}

# paq:
# {
#    'paq_name' : { 'dir' : dir, 'url' : url }
# }
final paqs = {}

# Paq management functions

export def Init(): void
    mkdir(pack.start_dir, 'p', 0o700)
    mkdir(pack.opt_dir,   'p', 0o700)

    for [name, args] in g:paqs
        const url = $'https://github.com/{name}'
        const as = args->get('as', name->split('/')[1])
        const dir = printf('%s/%s',
            args->get('opt', false) ? pack.opt_dir : pack.start_dir,
            as)

        paqs[as] = {dir: dir, url: url}
    endfor
enddef

export def Update(): void
    const max = paqs->mapnew((k, _) => len(k))->max()

    for [as, paq] in paqs->items()
        const [dir, url] = paq->values()
        const sed_rm_ansi_esc_chars = printf('%s %s %s',
            'sed',
            '-e "s/[^[:print:]]\\[[0-9;]*[a-zA-Z]//g"',
            '-e "s/[[:cntrl:]]//g"')

        # check if it has been git cloned already
        const git_cmd = finddir('.git', dir)->empty()
            ? printf('%s | %s',
                $'git clone --depth=1 {url} {dir}',
                sed_rm_ansi_esc_chars)
            : printf('%s && %s 2>&1 | %s',
                $'git -C {dir} fetch --depth=1',
                $'git -C {dir} reset --hard FETCH_HEAD',
                sed_rm_ansi_esc_chars)
        const git_cmd_result = system(git_cmd)->split('\n')

        const doc_dir = $'{dir}/doc'
        if doc_dir->isdirectory()
            execute 'helptags' doc_dir
        endif

        const fmt = $'%{max + 1}s: %s'
        echo printf(fmt, as, git_cmd_result[0])
    endfor
enddef

export def Clean(): void
    const dirs = [pack.start_dir, pack.opt_dir]
        ->mapnew((_, d) => glob($'{d}/*', true, true))
        ->flattennew()

    const managed = paqs->values()->mapnew((_, v) => v.dir)
    const orphaned = dirs->copy()->filter((_, d) => managed->index(d) == -1)

    if orphaned->empty()
        redraw! | echo $'Pack [{pack.name}] is clean.'
        return
    endif

    const prompt = [
        "The following folder(s) will be deleted.",
        "",
        orphaned->join("\n"),
        "",
        "Are you sure? [y/N] "
    ]->join("\n")

    echohl WarningMsg | echo "!!! WARNING !!!" | echohl None
    echo "\n"

    if 'y' == input(prompt)->tolower()
        orphaned->foreach((_, d) => delete(d, 'rf'))
    endif
enddef
