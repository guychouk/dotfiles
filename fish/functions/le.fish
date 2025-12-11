# Switch env from $LE_ENVS_DIR. Pair with direnv's .envrc for a decent solution
# switching environment variables.
function le
    ln -sf "$LE_ENVS_DIR/.env.$argv[1]" .env.current
end
