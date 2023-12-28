FROM ubuntu:20.04

# build time variables
ARG BUILD_DATE="000000"
ENV BUILD_DATE=${BUILD_DATE}
ARG BUILD_TAG="000000"
ENV BUILD_TAG=${BUILD_TAG}
ARG REPONAME="000000"
ENV REPONAME=${REPONAME}

RUN mkdir -p /opt2 && mkdir -p /data2
ENV TZ=America/New_York
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt update && apt-get -y upgrade
# Set the locale
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
		locales build-essential cmake cpanminus && \
	localedef -i en_US -f UTF-8 en_US.UTF-8 && \
	cpanm FindBin Term::ReadLine

# install basic dependencies with apt-get
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
  build-essential \
	g++ \
	gcc \
	gfortran \
	git \
	libatlas-base-dev \
	libblas-dev \
	libboost-dev \
	libbz2-dev \
	libcurl4-openssl-dev \
	libexpat1-dev \
	libfreetype6-dev \
	libgd-dev \
	libgd-perl \
	libglib2.0-dev \
  libgpgme11-dev \
	libgs-dev \
	libgsl-dev \
	libgsl0-dev \
	libhtml-template-compiled-perl \
	libicu-dev \
	libjudy-dev \
	liblapack-dev \
	liblzma-dev \
	libmysqlclient-dev \
	libncurses-dev \
	libopenmpi-dev \
	libpng-dev \
	librtmp-dev \
  libseccomp-dev \
	libssl-dev \
	libtool \
	libxml-libxml-debugging-perl \
	libxml-opml-simplegen-perl \
	libxml2-dev \
	libxslt-dev \
	make \
	manpages-dev \
	openjdk-17-jre-headless \
	parallel \
	pigz \
  pkg-config \
	rsync \
  squashfs-tools \
	unzip \
  uuid-dev \
	wget \
	zlib1g \
	zlib1g-dev \
	zlibc

# Install conda and give write permissions to conda folder
RUN echo 'export PATH=/opt2/conda/bin:$PATH' > /etc/profile.d/conda.sh && \
    wget --quiet "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh" -O ~/miniforge3.sh && \
    /bin/bash ~/miniforge3.sh -b -p /opt2/conda && \
    rm ~/miniforge3.sh && chmod 777 -R /opt2/conda/
ENV PATH="/opt2/conda/bin:$PATH"

# install devtools
RUN mamba install -c conda-forge r-base=4.3.2 r-devtools

# install R package
COPY . /opt2/reneeTools
RUN R -e 'devtools::install("/opt2/reneeTools")'

# cleanup
RUN apt-get clean && apt-get purge && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY Dockerfile /opt2/Dockerfile
RUN chmod -R a+rX /opt2/Dockerfile

WORKDIR /data2
