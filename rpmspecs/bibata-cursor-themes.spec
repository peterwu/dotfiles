%global         source_name Bibata_Cursor
%global         debug_package %{nil}
%undefine       _disable_source_fetch

Name:           bibata-cursor-themes
Version:        1.0.3
Release:        1%{?dist}
Summary:        OpenSource, Compact and Material Designed Cursor Set

License:        GNU General Public License v3.0
URL:            https://github.com/ful1e5/Bibata_Cursor
Source0:        %{url}/archive/v%{version}.tar.gz

BuildArch:      noarch

BuildRequires:  nodejs
BuildRequires:  yarnpkg
BuildRequires:  python3
BuildRequires:  libX11-xcb
BuildRequires:  gtk3
BuildRequires:  nss
BuildRequires:  mesa-libgbm
BuildRequires:  alsa-lib
Requires:       gtk3

%description
OpenSource, Compact and Material Designed Cursor Set

%prep
%setup -n %{source_name}-%{version}

%build
python3 -m pip install --upgrade pip                 # Update pip to latest
python3 -m pip install virtualenv                    # Install python virtual environment
virtualenv venv                                      # Create new virtualenv named `venv`
source venv/bin/activate                             # Activate virtualenv
yarn install                                         # Install all Node Packages
yarn py_install                                      # Install all PyPi Packages with Bibata builder
yarn render:bibata-modern                            # Render Bibata Modern Bitmaps
yarn render:bibata-original                          # Render Bibata Original Bitmaps
yarn build:x11                                       # Build only X11 cursors
deactivate

%install
%{__rm} -rf %{buildroot}
%{__mkdir} -p %{buildroot}%{_datadir}/icons
for theme in $(ls %{_builddir}/%{source_name}-%{version}/themes); do
  %{__mv} %{_builddir}/%{source_name}-%{version}/themes/${theme} %{buildroot}%{_datadir}/icons
  %{__chmod} 0755 %{buildroot}%{_datadir}/icons/${theme}
done

%clean
%{__rm} -rf %{buildroot}

%files
%license LICENSE
%doc README.md
%{_datadir}/icons/*

%changelog
* Tue Nov 17 09:42:13 EST 2020 Peter Wu <peterwu@hotmail.com>
- New Release - v1.0.3
