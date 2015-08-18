CREATE TABLE users (
  id SERIAL PRIMARY KEY NOT NULL,
  name TEXT NOT NULL UNIQUE,
  email TEXT NOT NULL UNIQUE
);

CREATE TABLE reminders (
  id SERIAL PRIMARY KEY NOT NULL,
  subject TEXT,
  body TEXT NOT NULL,
  send_at TIMESTAMP NOT NULL,
  sent BOOLEAN NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users(id)
);

CREATE TABLE tokens (
  id SERIAL PRIMARY KEY NOT NULL,
  token TEXT UNIQUE NOT NULL,
  read BOOLEAN NOT NULL,
  append BOOLEAN NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users(id)
);

CREATE TABLE codes (
  id SERIAL PRIMARY KEY NOT NULL,
  code TEXT UNIQUE NOT NULL,
  generated_at TIMESTAMP NOT NULL,
  valid BOOLEAN NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users(id)
);
