let
  plutus = import ../../. { };
  configurations = import ./configurations.nix;
  machines = (plutus.pkgs.lib.importJSON ./machines.json);
  promTargets = [
    { ip = machines.marloweDashA.ip; label = machines.marloweDashA.dns; port = 9100; }
    { ip = machines.marloweDashB.ip; label = machines.marloweDashB.dns; port = 9100; }
    { ip = machines.webghcA.ip; label = machines.webghcA.dns; port = 9100; }
    { ip = machines.webghcB.ip; label = machines.webghcB.dns; port = 9100; }
  ];
in
{
  "${machines.marloweDashA.dns}" = configurations.marloweDash "marlowe-dash-a";
  "${machines.marloweDashB.dns}" = configurations.marloweDash "marlowe-dash-b";
  "${machines.webghcA.dns}" = configurations.webGhc "web-ghc-a";
  "${machines.webghcB.dns}" = configurations.webGhc "web-ghc-b";
  "${machines.nixops.dns}" = configurations.prometheus { hostName = "prometheus"; environment = machines.environment; inherit promTargets; };
}
