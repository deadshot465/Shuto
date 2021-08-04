import needle from 'needle';

export const _twitterStreamConnect = (token: string) => (url: string) => (timeout: number) => () => {
    return needle.get(url, {
        headers: {
            'Authorization': `Bearer ${token}`,
        },
        timeout: timeout
    });
}