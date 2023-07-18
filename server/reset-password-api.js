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
    path: '/api/reset-password/:passwordResetCode',
    method: 'post',
    handler: async (req, res) => {
        const { passwordResetCode } = req.params;
        const { body } = req;
        const { password: newPassword } = body;

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

        const findUserByPasswordCode = async (parameters) => {
            const query = `SELECT * FROM ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users WHERE passwordresetcode = ? ALLOW FILTERING;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });

                return result;
            } catch (ex) {
                console.log('Error in finduser', ex.toString());
            }
        };
        const user = await findUserByPasswordCode([passwordResetCode]);

        if (!user?.rows?.length && !user?.rows[0]) {
            return res.sendStatus(401);
        }

        const { id, passwordhash, salt, email } = user.rows[0];
        const pepper = process.env.PEPPER_STRING;

        const isSame = await bcrypt.compare(salt + newPassword + pepper, passwordhash);

        if (isSame) return res.statusCode(404);

        const newSalt = uuid();
        const newPasswordHash = await bcrypt.hash(newSalt + newPassword + pepper, 10); // 2 arg is num of iterations to

        const updateUser = async (parameters) => {
            const query = `UPDATE ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users SET passwordresetcode = ?, passwordhash = ?, salt = ? WHERE id = ? IF EXISTS;`;
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
        } = await updateUser([null, newPasswordHash, newSalt, id]); // first param is re-setting password reset code in DB

        if (!applied['[applied]']) {
            console.log('Unsuccessfull in updating user');
            return res.sendStatus(404);
        }

        try {
            await sendEmail({
                to: email,
                from: 'dooshanstevanovic@gmail.com',
                subject: 'Password Reset successfully',
                text: `Your password has been reset`,
                html: `<div>
            <h1>Hello !</h1>
            <div>
              <h2>Password Reset Successfully</h2> 
              <p>Your password has been reset</p>
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
