%global         source_name Bibata_Cursor
%global         debug_package %{nil}

Name:           bibata-cursor-themes
Version:        1.0.3
Release:        1%{?dist}
Summary:        OpenSource, Compact and Material Designed Cursor Set

License:        MIT
URL:            https://github.com/ful1e5/Bibata_Cursor
%undefine _disable_source_fetch
Source0:        %{url}/archive/v%{version}.tar.gz

BuildArch:      noarch

BuildRequires:  nodejs
BuildRequires:  yarnpkg
BuildRequires:  python3
BuildRequires:  libX11-xcb
BuildRequires:  gtk3
Requires:       gtk3

%description
OpenSource, Compact and Material Designed Cursor Set

%prep
%autosetup -n %{source_name}-%{version}


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

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/%{_datadir}/icons
for theme in $(ls $RPM_BUILD_DIR/%{source_name}-%{version}/themes) ; do
  mv $RPM_BUILD_DIR/%{source_name}-%{version}/themes/${theme} $RPM_BUILD_ROOT/%{_datadir}/icons
  chmod 755 $RPM_BUILD_ROOT/%{_datadir}/icons/${theme}
done

%clean
rm -rf $RPM_BUILD_ROOT

%files
%license LICENSE
%doc README.md
%{_datadir}/icons/*


%changelog
* Tue Nov 17 09:42:13 EST 2020 Peter Wu <peterwu@hotmail.com>
- New Release - v1.0.3
