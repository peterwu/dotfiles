Name:           vim-symlinks
Version:        1.0.0
Release:        1%{?dist}

Summary:        Symlinks for vim
License:        MIT
Source0:        nothing

BuildArch:      noarch

%description
Create symlinks for vim


%package -n vi-vim-symlinks
Summary:        Replace vi & vim with vimx

BuildRequires:  bash
Requires:       vim-enhanced

Provides:       vi
Provides:       %{_bindir}/vi

Conflicts:      vi

%description -n vi-vim-symlinks
Replace vi with vim by linking relevant commands to vim


%package -n vi-vimx-symlinks
Summary:        Replace vi & vim with vimx

BuildRequires:  bash
Requires:       vim-X11

Provides:       vi
Provides:       vim
Provides:       %{_bindir}/vim

Conflicts:      vi
Conflicts:      vim

%description -n vi-vimx-symlinks
Replace vi & vim with vimx by linking relevant commands to vimx

%install
%{__rm} -rf %{buildroot}
%{__mkdir} -p %{buildroot}%{_bindir}
%{__mkdir} -p %{buildroot}%{_mandir}/man1

%{__ln_s} %{_bindir}/vimx %{buildroot}%{_bindir}/vim
%{__ln_s} %{_bindir}/vim %{buildroot}%{_bindir}/vi
%{__ln_s} %{_mandir}/man1/vim.1.gz %{buildroot}%{_mandir}/man1/vi.1.gz

for _f in ex rvi rview view; do
  %{__ln_s} %{_bindir}/vi %{buildroot}%{_bindir}/${_f}
  %{__ln_s} %{_mandir}/man1/vi.1.gz %{buildroot}%{_mandir}/man1/${_f}.1.gz
done

for _f in rvim vimdiff; do
  %{__ln_s} %{_bindir}/vim %{buildroot}%{_bindir}/${_f}
done

%clean
%{__rm} -rf %{buildroot}

%files -n vi-vim-symlinks
%{_bindir}/ex
%{_bindir}/rvi
%{_bindir}/rview
%{_bindir}/vi
%{_bindir}/view
%{_mandir}/man1/*

%files -n vi-vimx-symlinks
%{_bindir}/ex
%{_bindir}/rvi
%{_bindir}/rview
%{_bindir}/vi
%{_bindir}/view
%{_bindir}/rvim
%{_bindir}/vim
%{_bindir}/vimdiff
%{_mandir}/man1/*

%changelog
* Thu Nov 19 11:34:41 EST 2020 Peter Wu <peterwu@hotmail.com>
- 
