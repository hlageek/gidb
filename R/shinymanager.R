# ---- SETUP: Create Authentication tables ----
setup_shinymanager_tables <- function() {
  con <- dbConnect(
    dbDriver("PostgreSQL"),
    dbname = dbname,
    host = host,
    port = port,
    user = user,
    password = password
  )

  # Table 1: credentials (encrypted blob)
  dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS credentials (
      id INTEGER PRIMARY KEY DEFAULT 1,
      credentials BYTEA NOT NULL,
      CHECK (id = 1)
    )
  "
  )

  # Table 2: pwd_mngt (password management)
  dbExecute(
    con,
    "
    CREATE TABLE IF NOT EXISTS pwd_mngt (
      user VARCHAR(100) PRIMARY KEY,
      must_change BOOLEAN DEFAULT FALSE,
      have_changed BOOLEAN DEFAULT FALSE,
      date_change TIMESTAMP,
      n_wrong_pwd INTEGER DEFAULT 0
    )
  "
  )

  dbDisconnect(con)
  message("Tables created successfully")
}

# ---- Encryption/Decryption functions ----
write_postgres_encrypt <- function(conn, credentials_df, passphrase) {
  # Serialize the credentials dataframe
  credentials_serialized <- serialize(credentials_df, NULL)

  # Encrypt using AES
  key <- openssl::sha256(charToRaw(passphrase))
  credentials_encrypted <- openssl::aes_cbc_encrypt(credentials_serialized, key)

  # Convert to hex string for PostgreSQL
  blob_list <- as.list(credentials_encrypted)
  blob_hex <- paste0(
    "\\x",
    paste(sprintf("%02x", as.integer(blob_list)), collapse = "")
  )

  # Upsert into database
  dbExecute(
    conn,
    paste0(
      "
    INSERT INTO credentials (id, credentials)
    VALUES (1, '",
      blob_hex,
      "'::bytea)
    ON CONFLICT (id) DO UPDATE 
    SET credentials = EXCLUDED.credentials
  "
    )
  )

  invisible(TRUE)
}

read_postgres_decrypt <- function(conn, passphrase) {
  # Read encrypted blob
  result <- dbGetQuery(conn, "SELECT credentials FROM credentials WHERE id = 1")

  if (nrow(result) == 0) {
    stop("No credentials found in database")
  }

  # Get the blob (comes as a list of raw vectors)
  blob <- result$credentials[[1]]

  # Decrypt
  key <- openssl::sha256(charToRaw(passphrase))
  credentials_decrypted <- openssl::aes_cbc_decrypt(blob, key)

  # Unserialize back to dataframe
  credentials <- unserialize(credentials_decrypted)

  return(credentials)
}
