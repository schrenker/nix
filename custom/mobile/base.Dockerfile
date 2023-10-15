FROM localhost/systemd:latest

ARG DEBIAN_FRONTEND=noninteractive

ENV TERM=alacritty

# RUN useradd --uid 1000 --create-home engineer

# RUN echo 'engineer ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/engineer

# USER engineer

# WORKDIR /home/engineer

# Base
RUN apt update \
    && apt install -y --no-install-recommends \
       curl \
       gpg-agent \
       git \
    && apt clean \
    && rm -Rf /usr/share/doc &&  rm -Rf /usr/share/man \
    && rm -rf /var/lib/apt/lists/* \
    && touch -d "2 hours ago" /var/lib/apt/lists


# Emacs
RUN add-apt-repository ppa:ubuntuhandbook1/emacs

RUN apt install -y --no-install-recommends emacs-nox

RUN curl -sSL https://raw.githubusercontent.com/alacritty/alacritty/master/extra/alacritty.info | tic -x -

RUN git clone https://github.com/schrenker/emacs.d.git ~/.config/emacs

RUN mkdir -p ~/org
