FROM ubuntu:16.04

MAINTAINER Nick Lewchenko "nicholas.lewchenko@colorado.edu"


##### Add nix package manager
# (src is https://github.com/NixOS/docker)

# Enable HTTPS support in wget.
RUN apt-get update
RUN apt-get install -y openssl git wget
RUN apt-get install bzip2

# Download Nix and install it into the system.
RUN wget https://nixos.org/releases/nix/nix-2.0.4/nix-2.0.4-x86_64-linux.tar.bz2 \
  && echo "d6db178007014ed47ad5460c1bd5b2cb9403b1ec543a0d6507cb27e15358341f  nix-2.0.4-x86_64-linux.tar.bz2" | sha256sum -c \
  && tar xjf nix-*-x86_64-linux.tar.bz2
RUN mkdir /var/empty
RUN addgroup --gid 30000 --system nixbld \
  && for i in $(seq 1 30); do adduser --home /var/empty --system --no-create-home --disabled-password --gecos "Nix build user $i" --uid $((30000 + i)) --ingroup nixbld nixbld$i ; done \
  && mkdir -m 0755 /nix && USER=root sh nix-*-x86_64-linux/install \
  && ln -s /nix/var/nix/profiles/default/etc/profile.d/nix.sh /etc/profile.d/ \
  && rm -r /nix-*-x86_64-linux*

# Get a specific nixpkgs
RUN git clone https://github.com/nixos/nixpkgs /root/nixpkgs
RUN cd /root/nixpkgs && git checkout edd63e05d1e37ed3fb9623aca25c874014d9778c

ONBUILD ENV \
    ENV=/etc/profile \
    USER=root \
    PATH=/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt \
    NIX_SSL_CERT_FILE=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt

ENV \
    ENV=/etc/profile \
    USER=root \
    PATH=/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt \
    NIX_SSL_CERT_FILE=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt \
NIX_PATH=/root/nixpkgs


##### Set up Android Emulator
# (src is https://github.com/cuplv/docker-emulator-android)

# Set up insecure default key
ADD adbkey adbkey.pub adb_usb.ini /root/.android/

ENV LINK_ANDROID_SDK=https://dl.google.com/android/repository/sdk-tools-linux-3859397.zip \
    LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8 \
    ANDROID_HOME=/opt/android-sdk-linux \
    PATH="$PATH:/opt/android-sdk-linux/tools:/opt/android-sdk-linux/platform-tools:/opt/android-sdk-linux/tools/bin:/opt/android-sdk-linux/emulator"


RUN dpkg --add-architecture i386 && \
    echo "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe multiverse" > /etc/apt/sources.list && \
    echo "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list && \
    echo "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial-security main restricted universe multiverse" >> /etc/apt/sources.list && \
    echo "deb mirror://mirrors.ubuntu.com/mirrors.txt xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -yq software-properties-common libstdc++6:i386 zlib1g:i386 libncurses5:i386 \
                        locales ca-certificates apt-transport-https curl unzip redir iproute2 \
                        openjdk-8-jdk xvfb x11vnc fluxbox nano libpulse0 \
                        --no-install-recommends && \
    locale-gen en_US.UTF-8 && \

    # Install Android SDK
    curl -L $LINK_ANDROID_SDK > /tmp/android-sdk-linux.zip && \
    unzip -q /tmp/android-sdk-linux.zip -d /opt/android-sdk-linux/ && \
    rm /tmp/android-sdk-linux.zip && \

    # Customized steps per specific platform
    yes | sdkmanager --no_https --licenses && \
    sdkmanager --no_https emulator tools platform-tools "platforms;android-26" "system-images;android-26;google_apis;x86" --verbose && \
    echo no | avdmanager create avd -n "x86" --package "system-images;android-26;google_apis;x86" --tag google_apis && \

    # Unfilter devices (now local because CI downloads from github are unstable)
    # curl -o /root/.android/adb_usb.ini https://raw.githubusercontent.com/apkudo/adbusbini/master/adb_usb.ini && \

    # Optimize size of the image
    rm /root/.android/avd/x86.avd/userdata.img && \
    tar -czvf /opt/android-sdk-linux/system-images/android-26/google_apis/x86/userdata.img.tar.gz /opt/android-sdk-linux/system-images/android-26/google_apis/x86/userdata.img && \
    rm /opt/android-sdk-linux/system-images/android-26/google_apis/x86/userdata.img && \
    tar -czvf /opt/android-sdk-linux/system-images/android-26/google_apis/x86/system.img.tar.gz /opt/android-sdk-linux/system-images/android-26/google_apis/x86/system.img && \
    rm /opt/android-sdk-linux/system-images/android-26/google_apis/x86/system.img && \
    DEBIAN_FRONTEND=noninteractive apt-get purge -yq unzip openjdk-8-jdk && \

    apt-get -yq autoremove && \
    apt-get clean && \
    apt-get autoclean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ADD config.ini /root/.android/avd/x86.avd/config.ini

# # Expose adb
# EXPOSE 5037 5554 5555


##### Install server

WORKDIR /root

RUN nix-env -iA nixpkgs.graphviz
RUN nix-env -iA nixpkgs.cabal2nix
RUN nix-env -iA nixpkgs.androidsdk

# COPY ./_server ./server
# COPY ./exprs.nix ./exprs.nix

RUN git clone https://github.com/cuplv/droidstar-demo
# RUN cp droidstar-demo/docker-server/exprs.nix ./
ADD exprs.nix /root/exprs.nix
RUN nix-env -f exprs.nix -iA sbt-android

RUN cabal2nix droidstar-demo/server > server.nix
RUN nix-env -f exprs.nix -iA droidstar-demo-server

RUN git clone https://github.com/cuplv/droidstar
RUN cd droidstar && git checkout demo-config && sbt driverApp/android:package

EXPOSE 30025

# Add scripts
ADD start.sh /start.sh
RUN chmod +x /start.sh

ADD start-all.sh /start-all.sh
RUN chmod +x /start-all.sh

CMD /start-all.sh
