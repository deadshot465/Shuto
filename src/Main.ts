import { CommandClient, ClientOptions, Message, PossiblyUncachedTextableChannel } from 'eris';

const makeClient = (token: string, clientOptions: ClientOptions, prefix: string) => {
    return new CommandClient(token, clientOptions, {
        prefix: prefix
    });
};

export const _makeClient = (token: string) => (clientOptions: ClientOptions) => (prefix: string) => {
    return makeClient(token, clientOptions, prefix);
};

export const _connectClient = (client: CommandClient) => {
    return client.connect().then(async () => {
        console.log(`Hachiya Shuto is now online.`);
    });
};

export const _onMessageCreate = (client: CommandClient) => (callback: (msg: Message<PossiblyUncachedTextableChannel>) => void) => {
    return client.on('messageCreate', msg => callback(msg))
};