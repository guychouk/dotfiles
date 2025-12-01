# Switch env with LE_ENVS_DIR and direnv
# .envrc: dotenv_if_exists .env.current
function le
    ln -sf "$LE_ENVS_DIR/.env.$argv[1]" .env.current
end
