// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');

exports.handler = async function (req) {
    const { body, httpMethod } = req;
    let client;
    let parsedBody;

    try {
        client = await clientPromise;
    } catch (err) {
        console.log('Client: ', err);
        client = err.toString();
    }

    if (httpMethod !== 'POST')
        return {
            statusCode: 403,
        };

    if (typeof client?.execute !== 'function' || !client) {
        return {
            statusCode: 500,
            body: `{message: There is no client, payload: ${client}}`,
        };
    }

    try {
        parsedBody = JSON.parse(body);
    } catch (error) {
        return {
            statusCode: 500,
            body: error.toString(error),
        };
    }

    const { email, password } = parsedBody;
    // const astraClient = await createClient({
    //     astraDatabaseId: process.env.ASTRA_DB_ID,
    //     astraDatabaseRegion: process.env.ASTRA_DB_REGION,
    //     applicationToken: process.env.ASTRA_DB_APPLICATION_TOKEN,
    // });

    // const usersCollection = astraClient
    //     .namespace(process.env.ASTRA_DB_KEYSPACE)
    //     .collection('users');

    // const user = await usersCollection.findOne({ email: { $eq: email } });
    const findUser = async (parameters) => {
        const query = `SELECT * FROM ${
            process.env.NODE_ENV === 'development'
                ? process.env.ASTRA_DB_KEYSPACE
                : process.env.ASTRA_DB_KEYSPACE_PROD
        }.users WHERE email = ? ALLOW FILTERING;`;
        try {
            const result = client.execute(query, parameters, { prepare: true });

            return result;
        } catch (ex) {
            console.log('Error in finduser', ex.toString());
        }
    };
    const user = await findUser([email]);

    if (!user?.rows[0]) {
        return {
            statusCode: 401,
        };
    }

    const { id, isverified, passwordhash, salt, firstname, verificationstring, avatarurl } =
        user.rows[0];
    const pepper = process.env.PEPPER_STRING;

    const isCorrect = await bcrypt.compare(salt + password + pepper, passwordhash);

    if (isCorrect) {
        const token = jwt.sign(
            {
                id,
                isverified,
                email,
                firstname,
                verificationstring,
                profilepicurl: avatarurl || '',
            },
            process.env.JWT_SECRET,
            {
                expiresIn: '2h',
            },
        );

        if (!token)
            return {
                statusCode: 403,
            };

        return {
            statusCode: 200,
            body: JSON.stringify({ token }),
        };
    } else {
        return {
            statusCode: 401,
        };
    }
};
