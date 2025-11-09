{ options, config, lib, pkgs, ... }:

with lib;
with lib.nerv;
let
  cfg = config.nerv.opt.common.checks;
in
{
  options.nerv.opt.common.checks = with types; {
    enable = mkBoolOpt false "Whether or not to enable common assertions.";
    checkUpdateMicrocode = mkBoolOpt true "Whether or not to enable the updateMicrocode.";
  };

  config = mkIf cfg.enable {

    assertions = optional cfg.checkUpdateMicrocode
      {
        assertion = config.hardware.cpu.amd.updateMicrocode || config.hardware.cpu.intel.updateMicrocode;
        message = "common.checks: updateMicrocode should be set for intel or amd (config.hardware.cpu.{amd|intel}.updateMicrocode)";
      };
  };
}
