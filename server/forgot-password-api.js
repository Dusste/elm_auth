// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');
const sendgrid = require('@sendgrid/mail');
sendgrid.setApiKey(process.env.SENDGRID_API_KEY);

const sendEmail = ({ to, from, subject, text, html = '' }) => {
    const msg = { to, from, subject, text, html };
    return sendgrid.send(msg);
};

exports.module = {
    path: '/api/forgot-password',
    method: 'post',
    handler: async (req, res) => {
        const { body } = req;
        const { email } = body;

        let client;

        try {
            client = await clientPromise;
        } catch (err) {
            console.log('Client: ', err);
            client = err.toString();
        }

        if (!client || typeof client?.execute !== 'function') {
            return res.status(500).json(`{message: There is no client, payload: ${client}}`);
        }

        const passwordResetCode = uuid();

        const findUser = async (parameters) => {
            const query = `SELECT * FROM ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users WHERE email = ? ALLOW FILTERING;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });

                return result;
            } catch (ex) {
                console.log('Error in finduser', ex.toString());
            }
        };
        const user = await findUser([email]);

        if (!user?.rows?.length && !user?.rows[0]) {
            return res.sendStatus(401);
        }

        const {
            id: decodedId,
            isverified: isVerified,
            email: emailFromUser,
            verificationstring,
        } = user.rows[0];

        const updateUser = async (parameters) => {
            const query = `UPDATE ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users SET passwordresetcode = ? WHERE id = ? IF EXISTS;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });
                //  result would be undefined but query would be executed and entery is written in the DB
                return result;
            } catch (ex) {
                console.log('Error in createUser', ex.toString());
            }
        };

        const {
            rows: [applied],
        } = await updateUser([passwordResetCode, decodedId]);

        if (!applied['[applied]']) {
            console.log('Unsuccessfull in updating user');
            return res.sendStatus(404);
        }

        try {
            await sendEmail({
                to: email,
                from: 'dooshanstevanovic@gmail.com',
                subject: 'Password Reset',
                text: `To reset password click this link: ${
                    process.env.NODE_ENV === 'development'
                        ? 'http://localhost:8000'
                        : 'https://my-elm-app.netlify.app'
                }/password-reset/${passwordResetCode}`,
                html: `<div>
            <h1>Hello !</h1>
            <div>
              <h2>Password Reset </h2> 
              <p>To reset password click <a href="${
                  process.env.NODE_ENV === 'development'
                      ? 'http://localhost:8000'
                      : 'https://my-elm-app.netlify.app'
              }/password-reset/${passwordResetCode}">here</a></p>
            </div>
          </div>`,
            });
        } catch (err) {
            console.log('Error in sendEmail', err.toString());
            return {
                statusCode: 500,
            };
        }

        return res.sendStatus(200);
    },
};
