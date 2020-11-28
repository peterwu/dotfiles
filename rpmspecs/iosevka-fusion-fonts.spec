%global         source_name Iosevka
%global         debug_package %{nil}
%undefine       _disable_source_fetch

Name:           iosevka-fusion-fonts
Version:        4.0.0
Release:        1%{?dist}
Summary:        A custom font based on iosevka

License:        SIL Open Font License Version 1.1
URL:            https://github.com/be5invis/Iosevka
Source0:        %{url}/archive/v%{version}.tar.gz
Source1:        iosevka-fusion.toml

BuildArch:      noarch

BuildRequires:  npm
BuildRequires:  ttfautohint

%description
Based on Iosevka font, https://github.com/be5invis/Iosevka,
this font mixes elements from various fonts tailored to my personal taste.

%prep
%setup -n %{source_name}-%{version}
%{__cp} %SOURCE1 %{_builddir}/%{source_name}-%{version}/private-build-plans.toml

%build
npm install
npm run build -- ttf::iosevka-fusion

%install
%{__rm} -rf %{buildroot}
%{__install} -D -m 0644 %{_builddir}/%{source_name}-%{version}/dist/iosevka-fusion/ttf/*.ttf -t %{buildroot}%{_datadir}/fonts/%{name}

%clean
%{__rm} -rf %{buildroot}

%files
%license LICENSE.md
%doc README.md
%{_datadir}/fonts/*

%changelog
* Tue Nov 17 11:42:02 EST 2020 Peter Wu <peterwu@hotmail.com>
- New Release - v4.0.0
