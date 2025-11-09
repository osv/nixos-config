{ channels, ... }:

final: prev:

{
  openai-whisper-cpp = channels.unstable.openai-whisper-cpp.overrideAttrs (old: {
    buildInputs = (old.buildInputs or [ ]) ++ [
      channels.unstable.cudaPackages.cuda_cccl.dev # <nv/target>
    ];

  });
}
