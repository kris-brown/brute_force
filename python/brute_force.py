from db import create_int, ConnectInfo


def main() -> None:
    ci = ConnectInfo()
    conn = ci.connect()
    create_int(conn)


if __name__ == '__main__':
    main()
