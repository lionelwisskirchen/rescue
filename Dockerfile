FROM ubuntu:22.04

RUN apt update && apt upgrade
RUN apt install -y curl sudo git xz-utils

RUN adduser --disabled-password --gecos '' rescue 

RUN mkdir -m 0755 /nix && chown rescue /nix

ENV USER=rescue

WORKDIR /home/rescue
COPY . .
RUN mkdir /home/rescue/Library
COPY ./template/bibliography.bib ./Library/

RUN chown -R rescue:rescue .
RUN chown rescue:rescue ./Library/*
USER rescue
# Installation process
RUN curl -L https://nixos.org/nix/install | sh -s -- --no-daemon --yes
RUN mkdir -p ~/.config/nix/
RUN echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf 
RUN . ~/.nix-profile/etc/profile.d/nix.sh && nix run github:nix-community/home-manager -- switch --flake .#rescue

