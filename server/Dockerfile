FROM ubuntu:16.04


##### Install GHC
# (from https://github.com/freebroccolo/docker-haskell/blob/af83447feb6ebeabbc0cbad27adb3895e6262951/8.4/Dockerfile)

ENV LANG            C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg ca-certificates dirmngr curl git && \
    echo 'deb http://downloads.haskell.org/debian stretch main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 && \
    apt-get update && \
    apt-get install -y --no-install-recommends ghc-8.4.3 cabal-install-2.2 \
        zlib1g-dev libtinfo-dev libsqlite3-dev g++ netbase xz-utils make && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz -o stack.tar.gz && \
    curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz.asc -o stack.tar.gz.asc && \
    # apt-get purge -y --auto-remove curl && \
    export GNUPGHOME="$(mktemp -d)" && \
    gpg --keyserver ha.pool.sks-keyservers.net --recv-keys C5705533DA4F78D8664B5DC0575159689BEFB442 && \
    gpg --batch --verify stack.tar.gz.asc stack.tar.gz && \
    tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 && \
    /usr/local/bin/stack config set system-ghc --global true && \
    /usr/local/bin/stack config set install-ghc --global false && \
    rm -rf "$GNUPGHOME" /stack.tar.gz.asc /stack.tar.gz

ENV PATH /root/.cabal/bin:/root/.local/bin:/opt/cabal/2.2/bin:/opt/ghc/8.4.3/bin:$PATH


##### Install server

RUN apt-get install -y ttf-dejavu graphviz

RUN cabal update
RUN cabal install text wai wai-websockets warp websockets http-types safe containers mtl turtle aeson optparse-applicative foldl parsec
RUN cabal install dns
RUN cabal install process
RUN cabal install SHA bytestring
RUN cabal install stm



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


##### Build droidstar

WORKDIR /root

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update && \
    apt-get install -y openjdk-8-jdk sbt

RUN git clone https://github.com/cuplv/droidstar
RUN cd droidstar && git checkout demo-config && sbt driverApp/android:package


##### Build server

COPY ./_server ./server
RUN cd /root/server && cabal configure && cabal build
RUN cp /root/server/dist/build/droidstar-demo-server/droidstar-demo-server /bin/droidstar-demo-server
EXPOSE 30025


##### Re-install tools

RUN apt-get update && apt-get install -y unzip

RUN rm -r /opt/android-sdk-linux/tools

RUN curl -L $LINK_ANDROID_SDK > /tmp/android-sdk-linux.zip && \
    unzip -q /tmp/android-sdk-linux.zip -d /opt/android-sdk-linux/ && \
    rm /tmp/android-sdk-linux.zip && \

    # Customized steps per specific platform
    yes | sdkmanager --no_https --licenses && \
    yes | sdkmanager --no_https platform-tools "platforms;android-26" --verbose

# RUN cp /opt/android-sdk-linux/platform-tools/adb /bin/adb

RUN which adb


##### Add scripts

ADD start.sh /start.sh
RUN chmod +x /start.sh

ADD start-all.sh /start-all.sh
RUN chmod +x /start-all.sh

CMD ["/start-all.sh"]
