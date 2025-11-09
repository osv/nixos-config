{ lib, pkgs, config, virtual, ... }:

let
  inherit (lib) mkIf mkEnableOption optional;
  inherit (lib.nerv) mkOpt;

  cfg = config.nerv.opt.security.dns;
in {
  options.nerv.opt.security.dns = with lib.types; {
    enable = mkEnableOption "Enables Encrypted DNS";
  };

  config = mkIf cfg.enable {
    networking = {
      nameservers = [ "127.0.0.1" "::1" ];
      # If using dhcpcd:
      dhcpcd.extraConfig = "nohook resolv.conf";
      # If using NetworkManager:
      networkmanager.dns = "none";
    };

    nerv.opt.persist.state.directories = [{
      directory = "/var/lib/dnscrypt-proxy";
      user = "dnscrypt-proxy2";
      group = "dnscrypt-proxy2";
    }];

    systemd.services.dnscrypt-proxy2.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "dnscrypt-proxy2";
      Group = "dnscrypt-proxy2";
    };

    users.users.dnscrypt-proxy2 = {
      isSystemUser = true;
      createHome = false;
      group = "dnscrypt-proxy2";
      description = "DNSCrypt user";
    };

    users.groups.dnscrypt-proxy2 = { };

    services.dnscrypt-proxy2 = {
      enable = true;

      settings = {
        # https://github.com/carjorvaz/nixos/blob/d09a4b06ca797ee0a31e0c5dcfdd8b4cccdfe94f/profiles/dns/dnscrypt-proxy2.nix#L12

        # first (always pick the fastest server in the list)
        # p2 (randomly choose between the top 2 fastest servers)
        # ph (randomly choose between the top fastest half of all servers)
        # random (just pick any random server from the list)
        lb_strategy = "p3";

        # ipv6_servers = true;
        # Server must support DNS security extensions (DNSSEC)
        require_dnssec = true;
        ## Enable *experimental* support for HTTP/3 (DoH3, HTTP over QUIC)
        ## Note that, like DNSCrypt but unlike other HTTP versions, this uses
        ## UDP and (usually) port 443 instead of TCP.
        http3 = true;

        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy/public-resolvers.md";
          refresh_delay = 4 * 24;
          minisign_key =
            "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };

        bootstrap_resolvers = [ "9.9.9.11:53" "8.8.8.8:53" ];

        # you can choose a specific set of servers from https://github.com/DNSCrypt/dnscrypt-resolvers/blob/master/v3/public-resolvers.md
        server_names = [
          # "adguard-dns-unfiltered-ipv6"
          "cloudflare"
          # "cloudflare-ipv6"
          # "controld-uncensored"
          "mullvad-doh" # Public non-filtering, non-logging (audited), DNSSEC-capable, DNS-over-HTTPS resolver hosted by VPN provider Mullvad Anycast IPv4/IPv6 with servers in SE, DE, UK, US, AU and SG
          "techsaviours.org-dnscrypt" # No filter | No logs | DNSSEC | Nuremberg, Germany (netcup) | Maintained by https://techsaviours.org/

          "nextdns" # DNSSEC, Anycast, Non-logging, NoFilters
          # "nextdns-ipv6"
          "nextdns-ultralow" # NextDNS is a cloud-based private DNS service that gives you full control over what is allowed and what is blocked on the Internet.

          # DNSSEC/Non-logged/Uncensored in Amsterdam - DEV1-S instance donated by Scaleway.com Maintained by Frank Denis -
          "scaleway-ams"

          # https://cryptostorm.is/
          "cs-czech"
          "cs-hungary"
          "cs-bulgaria"
          "cs-poland"
          "dnscry.pt-chisinau-ipv4" # DNSCry.pt Chișinău - no filter, no logs, DNSSEC support (IPv4 server)
          "dnscry.pt-warsaw-ipv4"
          "dnscry.pt-vienna-ipv4"
          "dct-at1"
          "controld-unfiltered"
        ];

        cloaking_rules = with lib;
          let
            inherit (config.networking) hosts;
            entryToCloak = addr:
              concatMapStringsSep "\n" (hostname: "${hostname} ${addr}")
              hosts.${addr};
          in builtins.toFile "cloaking-rules.txt"
          (concatMapStringsSep "\n" entryToCloak
            (attrNames config.networking.hosts));
      };
    };
  };
}
