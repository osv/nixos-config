{ pkgs, ... }:

pkgs.mkShell {
  packages = with pkgs; [
    awscli2
    pv
    pass
  ];

  shellHook = ''
    # Extract AWS credentials from pass store
    if command -v pass >/dev/null 2>&1; then
      echo "Loading AWS credentials from pass store..."
      
      # Get the full credential data
      cred_data=$(pass show aws/kz_olexandr.sydorchuk.cli 2>/dev/null || echo "")
      
      if [ -n "$cred_data" ]; then
        # Extract secret (first line)
        export AWS_SECRET_ACCESS_KEY=$(echo "$cred_data" | head -n1)
        
        # Extract key ID
        export AWS_ACCESS_KEY_ID=$(echo "$cred_data" | grep "^key:" | cut -d' ' -f2)
        
        # Extract region
        export AWS_DEFAULT_REGION=$(echo "$cred_data" | grep "^region:" | cut -d' ' -f2)
        
        echo "AWS credentials loaded successfully"
        echo "Region: $AWS_DEFAULT_REGION"
        echo "Access Key ID: $${AWS_ACCESS_KEY_ID:0:10}..."
      else
        echo "Warning: Could not load AWS credentials from pass store"
        echo "Make sure 'kz_olexandr.sydorchuk.cli.gpg' exists in your password store"
      fi
    else
      echo "Warning: pass command not found"
    fi
    
    echo "AWS development environment ready"
    echo "Available tools: aws, pv, pass"
  '';
}
